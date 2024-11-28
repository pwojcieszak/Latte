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
### Statement w if
Ciało warunku może być blokiem albo dowolnym innym statementem (nic nowego). W przypadku statementu przypisania nowej wartości do zmiennej traktuję ją jako zmienną lokalną warunku (pomimo nie bycia wprost w nowym bloku). Przykład: w poniższym kodzie nadpisuję wartość 'y' w ciele warunku na obu jego odnogach. Pomimo tego, wartość 'y' po wyjściu z warunku dalej równa się 1.

```
int y = 1;
if (true)
    y = 2;
else
    y = 3;
printInt(y) // 1
```


### RelOps
Jako operatory relacyjne rozumiem "<", "<=", ">", ">=", "==", "!=".  
Inty obsługują wszystkie z nich. String i Bool obsługują tylko "==", "!=".