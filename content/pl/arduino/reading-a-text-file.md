---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Arduino: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Co & Dlaczego?

Przeczytanie pliku tekstowego to nic innego jak odczytanie zawartości tekstu z pliku i wykorzystanie jej do programu. Programiści korzystają z tej funkcji, aby mieć dostęp do danych, takich jak ustawienia, pomiary lub instrukcje dla urządzeń zewnętrznych.

# Jak to zrobić:

Arduino ma wbudowaną funkcję ```File.read()```, która odczytuje zawartość pliku tekstowego i zapisuje ją do zmiennej. Można to zrobić w następujący sposób:

```
File plik = SD.open("nazwa_pliku.txt");  // otwiera plik do odczytu
String zawartosc = plik.readString();  // zapisuje zawartość pliku do zmiennej
Serial.println(zawartosc);  // wyświetla zawartość na monitorze szeregowym
plik.close();  // zamyka plik
```

Pamiętaj, aby dodać odpowiednią bibliotekę SD, jeśli plik znajduje się na karcie SD.

# Ciekawostki:

Funkcja odczytu pliku istnieje od początku istnienia Arduino, ale pierwotnie była używana tylko do odczytu plików .hex na płytkach. Wraz z rozwojem i popularnością platformy, pojawiły się różne alternatywy do odczytu plików, takie jak SD.h czy SPI.h.

# Zobacz też:

https://www.arduino.cc/en/Reference/FileRead
https://www.arduino.cc/en/Tutorial/FileReadWrite
https://www.arduino.cc/en/Tutorial/Files