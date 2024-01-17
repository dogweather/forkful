---
title:                "Pisanie pliku tekstowego"
html_title:           "Arduino: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie pliku tekstowego to proces zapisywania danych lub informacji w postaci tekstu. Programiści często robią to, aby przechowywać, przesyłać lub przechwytywać informacje potrzebne do działania ich programów.

## Jak to zrobić:

Do napisania pliku tekstowego w ARDUINO możemy użyć funkcji `File.write()` lub `File.println()`. Oba służą do zapisywania komunikatów do pliku w pamięci mikrokontrolera.

Przykładowe użycie:

```
ArduinoFile plik = SD.open("dane.txt", FILE_WRITE);
plik.print("Witaj!");
```

W tym przykładzie otwieramy plik "dane.txt" w trybie zapisu i używamy funkcji `print()` by zapisać tekst "Witaj!" do tego pliku.

## Przejrzyjmy szczegóły:

Historia: Pisanie plików tekstowych jest jednym z podstawowych procesów programowania, używanym od lat do przechowywania danych. W ARDUINO wykorzystuje się to do zapisywania wyników z czujników lub komunikatów z programu.

Alternatywy: Podobną funkcjonalność do tworzenia plików tekstowych w ARDUINO oferują również inne platformy programowania, takie jak Raspberry Pi czy Python.

Szczegóły implementacji: Funkcja `File.write()` pozwala na bezpośrednie zapisywanie danych do pliku, natomiast `File.println()` automatycznie dodaje znaki nowej linii po każdej operacji zapisu.

## Zobacz także:

- Przykład zapisywania danych do pliku tekstowego w ARDUINO: https://www.arduino.cc/en/Tutorial/ReadWrite
- Przykładowy kod zapisujący komunikaty z czujnika do pliku tekstowego: https://lastminuteengineers.com/arduino-sd-card-interface-tutorial/