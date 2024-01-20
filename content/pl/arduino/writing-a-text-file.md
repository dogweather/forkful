---
title:                "Zapisywanie pliku tekstowego"
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zapis do pliku tekstowego oznacza umieszczanie danych w standardowym formacie tekstowym na nośnikach pamięci, takich jak karty SD. Programiści robią to, aby zachować informacje między sesjami, udostępnić dane lub zalogować zdarzenia.

## Jak to zrobić:
```Arduino
#include <SPI.h>
#include <SD.h>

File mojPlik;

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(4)) {
    Serial.println("Błąd inicjalizacji karty SD.");
    return;
  }
  Serial.println("Karta SD gotowa.");

  mojPlik = SD.open("test.txt", FILE_WRITE);
  
  if (mojPlik) {
    mojPlik.println("Witaj, świecie!");
    mojPlik.close();
    Serial.println("Zapisano do pliku: test.txt");
  } else {
    Serial.println("Błąd otwarcia pliku.");
  }
}

void loop() {
  // Nic więcej tutaj nie potrzebujemy.
}
```

Oczekiwany wynik:
```
Karta SD gotowa.
Zapisano do pliku: test.txt
```

## Głębsze spojrzenie
Pisanie do plików tekstowych jest popularne od wczesnych lat informatyki. Ewolucja nośników danych, od taśm magnetycznych po karty SD, wpłynęła na łatwość i szybkość tej operacji. Alternatywą jest zapis do EEPROM-u Arduino lub wykorzystanie innych rodzajów pamięci, np. pamięci Flash z modułów SPI. Implemetacja polega na użyciu odpowiednich bibliotek, jak SPI.h czy SD.h, które obsługują komunikację i funkcje plikowe.

## Zobacz również
- [Dokumentacja Arduino – biblioteka SD](https://www.arduino.cc/en/Reference/SD)
- [Podręcznik Arduino – zapis na karty SD](https://www.arduino.cc/en/Guide/Libraries#toc4)
- [Tutorial Sparkfun – Użycie kart SD w Arduino](https://learn.sparkfun.com/tutorials/using-the-serial-7-segment-display/all)