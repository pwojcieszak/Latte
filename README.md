# Parser Latte
Projekt parsera razem z analizą semantyczną dla kompilatora Latte.

## Kompilacja i uruchamianie programu
Projekt kompilujemy przy pomocy Makefile używając komendy `make`. W korzeniu projektu pojawi się plik wykonywalny latc_llvm.

W celu uruchomienia programu należy podać na wejściu programu plik z wyrażeniami zgodnymi z gramatyką Latte. Przykład uruchomienia:

`./latc_llvm ./lattests/good/core001.lat`

## Używane narzędzia i biblioteki
 - BNFC - 2.9.4
 - GHC - 9.0.2
 - clang - 14.0.6


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

## Optymalizacje
### Martwy kod
Na chwilę obecną jedyny martwy kod jaki usuwam to kod w blokach po instrukcjach return. Optymalizacji dokonuję po analizie semantycznej, dlatego martwy kod musi być semantycznie zgodny.