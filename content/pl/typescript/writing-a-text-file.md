---
title:                "TypeScript: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego?

Pisanie plików tekstowych jest nieodłączną częścią programowania, występuje w wielu różnych językach programowania, w tym również w TypeScript. Pisanie plików tekstowych jest ważne dla twórców oprogramowania, ponieważ dają one możliwość przechowywania i przetwarzania informacji w trwałej i czytelnej formie.

## Jak to zrobić?

Pisanie plików tekstowych w TypeScript jest proste i zastosowanie kilku prostych kroków pozwoli nie tylko utworzyć plik tekstowy, ale również manipulować jego zawartością. Aby rozpocząć, należy wykonać następujące czynności:

1. Importowanie modułu "fs", który zapewnia dostęp do funkcji związanych z systemem plików.
2. Utworzenie zmiennej, która będzie odpowiadać za nazwę pliku.
3. Użycie funkcji "writeFileSync" do utworzenia nowego pliku tekstowego i zapisania w nim zawartości.
4. Manipulacja zawartością pliku przy użyciu funkcji "appendFileSync" lub "writeFileSync".

Oto przykładowy kod w TypeScript:

```TypeScript
import * as fs from 'fs';

// nazwa pliku
let nazwaPliku = "moj_plik.txt";

// tworzenie i zapisywanie pliku
fs.writeFileSync(nazwaPliku, "Przykładowa zawartość pliku.");

// dodawanie treści do pliku
fs.appendFileSync(nazwaPliku, " Kolejna linia tekstu.");
```

Po wykonaniu tych czynności, plik "moj_plik.txt" powinien zostać utworzony w bieżącym katalogu i będzie zawierał następującą zawartość:

Przykładowa zawartość pliku. Kolejna linia tekstu.

## Deep Dive

Pisząc plik tekstowy w TypeScript, istnieje wiele funkcji, które można wykorzystać do manipulowania zawartością pliku. Poniżej przedstawiono kilka przydatnych przykładów:

1. Funkcja "readFileSync" pozwala na odczytanie zawartości istniejącego pliku tekstowego.
2. Funkcja "renameSync" służy do zmiany nazwy lub przeniesienia pliku do innego katalogu.
3. Funkcja "unlinkSync" usuwa istniejący plik tekstowy.

Warto również zwrócić uwagę na dodatkowe opcje, które można wykorzystać przy tworzeniu lub edycji pliku, takie jak kodowanie znaków, tryb dostępu czy flagi.

## Zobacz również

1. Dokumentacja TypeScript dotycząca funkcji związanych z systemem plików: https://www.typescriptlang.org/docs/handbook/file-system-functions.html
2. Przykłady wykorzystania funkcji systemu plików w TypeScript: https://www.tutorialspoint.com/typescript/typescript_file_system.htm
3. Tutorial "Pisanie plików w TypeScript": https://www.digitalocean.com/community/tutorials/how-to-use-the-readfile-function-of-node-js-to-read-the-content-of-a-file#reading-files-with-javascript