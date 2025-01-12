#!/bin/bash

# Ścieżka do folderu z testami
TEST_FOLDER="./mrjp-tests-master/good/basic/"

# Sprawdzenie, czy folder istnieje
if [ ! -d "$TEST_FOLDER" ]; then
    echo "Folder $TEST_FOLDER nie istnieje."
    exit 1
fi

# Znajdowanie wszystkich plików .lat w folderze i sortowanie ich
LATTE_FILES=$(find "$TEST_FOLDER" -type f -name "*.lat" | sort)

# Sprawdzenie, czy znaleziono jakiekolwiek pliki
if [ -z "$LATTE_FILES" ]; then
    echo "Brak plików .lat w folderze $TEST_FOLDER."
    exit 1
fi

# Iteracja po znalezionych plikach .lat
for FILE in $LATTE_FILES; do
    BASE_NAME=$(basename "$FILE" .lat) # Nazwa pliku bez rozszerzenia
    INPUT_FILE="$TEST_FOLDER/$BASE_NAME.input" # Plik wejściowy
    OUTPUT_FILE="$TEST_FOLDER/$BASE_NAME.output" # Plik wyjściowy

    echo "Testowanie pliku: $FILE"

    # Uruchomienie kompilatora latc_llvm
    ./latc_llvm "$FILE" > "program_output.txt"
    if [ $? -ne 0 ]; then
        echo "  Kompilacja nie powiodła się dla pliku $FILE"
        continue
    fi

    # # Wykonanie programu
    # if [ -f "$INPUT_FILE" ]; then
    #     # Jeśli istnieje plik wejściowy, przekazujemy go do programu
    #     ./a.out < "$INPUT_FILE" > program_output.txt
    # else
    #     # W przeciwnym razie uruchamiamy bez wejścia
    #     ./a.out > program_output.txt
    # fi

    # Porównanie wyniku z plikiem wyjściowym
    if [ -f "$OUTPUT_FILE" ]; then
        if diff -q program_output.txt "$OUTPUT_FILE" > /dev/null; then
            echo "  Test zakończony sukcesem."
        else
            echo "  Test nie powiódł się. Różnice:"
            diff program_output.txt "$OUTPUT_FILE"
        fi
    else
        echo "  Brak pliku wyjściowego $OUTPUT_FILE. Pomijanie porównania."
    fi

    # Usunięcie wygenerowanego pliku tymczasowego
    rm -f program_output.txt
done
