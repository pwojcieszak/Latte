#!/bin/bash

# Ścieżka do folderu z testami
TEST_FOLDER="./lattests/bad/"
OUTPUT_FILE="badResults"

# Sprawdzenie, czy folder istnieje
if [ ! -d "$TEST_FOLDER" ]; then
    echo "Folder $TEST_FOLDER nie istnieje."
    exit 1
fi

# Sprawdzenie, czy istnieje plik wynikowy i wyczyszczenie go
> "$OUTPUT_FILE"

# Znajdowanie wszystkich plików .lat w folderze i sortowanie ich
LAT_FILES=$(find "$TEST_FOLDER" -type f -name "*.lat" | sort)

# Sprawdzenie, czy znaleziono jakiekolwiek pliki
if [ -z "$LAT_FILES" ]; then
    echo "Brak plików .lat w folderze $TEST_FOLDER."
    exit 1
fi

# Iteracja po znalezionych i posortowanych plikach
for FILE in $LAT_FILES; do
    # Wykonaj latc_llvm dla pliku
    echo "Testowanie pliku: $FILE" >> "$OUTPUT_FILE"
    ./latc_llvm "$FILE" >> "$OUTPUT_FILE" 2>&1
    echo "--------------------------------------" >> "$OUTPUT_FILE"
done

echo "Wyniki zostały zapisane w pliku $OUTPUT_FILE."
