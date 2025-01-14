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
### Martwy kod
Na chwilę obecną jedyny martwy kod jaki usuwam to kod w blokach po instrukcjach return. Optymalizacji dokonuję po analizie semantycznej, dlatego martwy kod musi być semantycznie zgodny.

### Skoki w gałęziach If i While
Jeśli w ciele (bez sprawdzania zgnieżdżonych bloków) gałęzi warunku jest return to nie dodaję skoku do bloku końcowego i krawędzi między tymi blokami na grafie przepływu sterowania.

### Przypisania jednoargumentowe
Po wygenerowaniu kodu dokonuję analizy przypisań prostych. W przypadku przypisania gdzie po prawej stronie jest jeden argument prosty (zmienna, stała) wyszukuję użycia zmiennej LHS w kolejnych liniach i zastępuję jej wystąpienia RHS.
