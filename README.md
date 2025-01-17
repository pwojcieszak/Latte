# Kompilator Latte do LLVM
Projekt kompilatora Latte do LLVM zawierający parser, analizę semantyczną i generator wraz z optymalizacjami.

## Kompilacja i uruchamianie programu
Projekt kompilujemy przy pomocy Makefile używając komendy `make`. W korzeniu projektu pojawi się plik wykonywalny latc_llvm.

W celu uruchomienia programu należy podać na wejściu programu plik z wyrażeniami zgodnymi z gramatyką Latte. Przykład uruchomienia:

`./latc_llvm ./lattests/good/core001.lat`

## Używane narzędzia i biblioteki
 - BNFC - 2.9.4
 - GHC - 9.0.2
 - clang - 14.0.6

## Struktura katalogów
```
.
.
├── lib
│   └── runtime.c
├── Makefile
├── README.md
└── src
    ├── AbsLatte.hs
    ├── Backend.hs
    ├── Frontend.hs
    ├── Latte.cf
    ├── LexLatte.hs
    ├── MainLatte.hs
    ├── Midend.hs
    ├── ParLatte.hs
    └── PrintLatte.hs
```


W /src znajdują się przede wszystkim główne pliki uruchomieniowe dla kompilatorów:
  - MainLatte.hs - punkt wejściowy programu przekazujący pliki wejściowe do parsera, wywołujące generator, tworzące pliki wyjściowe,
  - Frontend.hs - frontend kompilatora
  - Backend.hs - backend kompilatora
  - Midend.hs - optymalizacje dokonywane w po skończeniu pracy Frontendu ale przed Backendem. Obecnie to tylko usuwanie martwego kodu
  - Optimizations.hs - optymalizacje LCSE i GCSE (w trakcie prac)
  - Latte.cf - gramatyka języka Latte
  - Reszta plików to wygenerowane przez BNFC pliki parsera języka Latte

W katalogu /lib w pliku runtime.c znajdują się funkcje biblioteczne zapisane w języku C. Lista funkcji: 
  - void printInt(int)           -- wypisuje Int
  - void printString(string)     -- wypisuje String
  - void error()                 -- wypisuje komunikat o błędzie i zatrzymuje program
  - int readInt()                -- wczytuje Int z stdin
  - string readString()          -- wczytuje String z stdin
  - char* concat(char*,char*)    -- konkatynuje dwa Stringi

Po zbudowaniu w korzeniu projektu pojawi się plik wykonywalny latc_llvm. Po uruchomieniu programu w folderze z plikiem wejściowym zostaną wygenerowane pliki .ll i .bc.

## Komentarze
### Statement w instrukcjach warunkowych
Ciało warunku może być blokiem albo dowolnym innym statementem (nic nowego). Odwołanie do y w odnogach instrukcji jest odwołaniem do "starego" y, którego wartość ma się zmienić. Oczywiście można utworzyć nowe zmienne lokalne instrukcjami int y = 2, int y =3, wtedy y jest lokalny i nie zmienia "starego" y, które przez cały czas jest równe 1.

```
int y = 1;
if (true)
    y = 2;
else
    y = 3;
printInt(y) // 2
```

### Funkcja error()
Funkcja void error() niweluje potrzebę następstwa komendy return.

### RelOps
Jako operatory relacyjne rozumiem "<", "<=", ">", ">=", "==", "!=".  
Inty obsługują wszystkie z nich. String i Bool obsługują tylko "==", "!=".

### Porównywanie typu String
Wartości typu String porównywane są na podstawie referencji.

### removeLastAssignment
Jest to funkcja wykorzystywana na końcu przetwarzania SExp czyli wyrażenia mającego nie być przypisanym do niczego. Funkcja modyfikuje ostatnio wygenerowaną linię usuwając przypisanie. Wyjątkiem jest sytuacja kiedy ostatnią instrukcją jest wyrażenie Phi. Ze względu na pracujący potem parser tekstu wykrywający tylko Phi z przypisaniem, zdecydowałem się nie usuwać przypisania w tym przypadku. Cała ta linia i tak zostanie usunięta na etapie optymalizacji, kiedy to zostanie uznana za przypisanie do nieużywanej zmiennej.

### Return a pętla while(true)
Zakładając, że mamy program z pętlą "while(true)", która kończy działanie programu po wykryciu na wejściu oczekiwanego znaku i tak wymagam od niej aby posiadała gwarantowany (czyli statycznie osiągalny) return.

## Optymalizacje
### Skoki w gałęziach If i While
Jeśli w ciele (bez sprawdzania zgnieżdżonych bloków) gałęzi warunku jest return to nie dodaję skoku do bloku końcowego i krawędzi między tymi blokami na grafie przepływu sterowania.

### Copy propagation
Po wygenerowaniu kodu dokonuję analizy przypisań prostych. W przypadku przypisania gdzie po prawej stronie jest jeden argument prosty (zmienna, stała) wyszukuję użycia zmiennej LHS w kolejnych liniach i zastępuję jej wystąpienia RHS. 

### LCSE
LCSE dokonuję w następujący sposób. Na wejściu mam mapę {NazwaBloku -> [Kod]}. Dla każdego bloku z osobna podmieniam użycia zmiennych o tej samej RHS (podmienione deklaracje usuwam). Mapa podmienionych zmiennych jest wspólna dla wszystkich bloków z powodu funkcji PHI, która może odwoływać się do zmiennej, którą w tym innym bloku lokalnie podmieniliśmy. Bloki te nie muszą następować po sobie. Dodatkowo jeden krok LCSE może odsłonić nowe miejsca do poprawy. Z tych dwóch powodów konieczne jest przejście LCSE więcej niż jeden raz. W swoim algorytmie dokonuję optymalizacji LCSE tak długo jak wynikowy kod różni się od wejściowego.

Nie optymalizuję wywołań funkcji ze względu na możliwość użycia w nich funkcji czytającej ze standardowego wejścia.

### GCSE
W GCSE ważne jest śledzenie przepływu bloków. Szczególnym przypadkiem jest rozgałęzienie się ścieżki pod wpływem warunku, np:

```
COND:
  br i1 %foo, label %TRUE, label %END
TRUE:
  %t_1 = add 2, 1
  %a = %t2
END:
  %t_2 = add 2, 1
  %b = %t2
```

W tym przypadku do bloku "END" prowadzi zarówno blok "COND" jak i blok "TRUE". Przechodząc bloki po kolei moglibyśmy zastąpić %t_2 przez %t_1 ale nie mamy pewności, że podczas działania programu faktycznie przejdziemy przez blok "TRUE". Użycie "%t_1" w bloku END nie jest zdominowane. 

Podobnie jest w przypadku wyrażeń boolowskich:

```
boolean a = 2>3 || 5>1;
```

Pierwszy warunek OR (analogicznie dla AND) wykona się zawsze dlatego stanie się wzorcem do zastąpienia. Drugi wykonuje się tylko warunkowo dlatego nie będzie zapisany we wzorcach.  
Przykłady:
```
boolean a = 2>3 || 5>1;
boolean b = 5>1 || 2>3;
```

Część a:
```
  %t3.0 = icmp sgt i32 2, 3
  br i1 %t3.0, label %L1_exp_true, label %L2_or_mid_true
L2_or_mid_true:
  %t4.0 = icmp sgt i32 5, 1
  br i1 %t4.0, label %L1_exp_true, label %L1_exp_false
```

Część b:
```
%t6.0 = icmp sgt i32 5, 1
  br i1 %t6.0, label %L3_exp_true, label %L4_or_mid_true
L4_or_mid_true:
  br i1 %t3.0, label %L3_exp_true, label %L3_exp_false
```

### Eliminacja martwego kodu
Martwy kod po instrukcjach return usuwam zaraz po analizie semantycznej.

Zbędne Phi ze względu na jednakowe wartości z poprzedzających bloków albo równe LHS (while_stmt -> while_cond) propaguję jako kopię.

Nieużywane zmienne eliminuje podczas kolejnych iteracji GCSE. Nie wykonywałem tego podczas LCSE ponieważ zmienna może nie mieć użycia w swoim bloku ale być używana w kolejnym.
