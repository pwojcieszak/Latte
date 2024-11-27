#!/bin/bash

# Ścieżka do folderu z testami
TEST_FOLDER="./lattests/good/"

# Sprawdzenie, czy folder istnieje
if [ ! -d "$TEST_FOLDER" ]; then
    echo "Folder $TEST_FOLDER nie istnieje."
    exit 1
fi

# Znajdowanie wszystkich plików .latte w folderze i sortowanie ich
LATTE_FILES=$(find "$TEST_FOLDER" -type f -name "*.lat" | sort)

# Sprawdzenie, czy znaleziono jakiekolwiek pliki
if [ -z "$LATTE_FILES" ]; then
    echo "Brak plików .latte w folderze $TEST_FOLDER."
    exit 1
fi

# Iteracja po znalezionych plikach
for FILE in $LATTE_FILES; do
    # Wykonaj latc_llvm i przefiltruj błędy semantyczne
    ERRORS=$(./latc_llvm "$FILE" 2>&1 | grep "Semantic Error:")
    
    # Jeśli znaleziono błędy, wypisz je
    if [ ! -z "$ERRORS" ]; then
        echo "Testowanie pliku: $FILE"
        echo "$ERRORS"
        echo "--------------------------------------"
    fi
done
