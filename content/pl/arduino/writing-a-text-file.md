---
title:                "Arduino: Pisanie pliku tekstowego"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego jest kluczowym elementem w programowaniu Arduino, ponieważ umożliwia zapisywanie danych i ułatwia przechowywanie informacji. Może to być użyteczne przy korzystaniu z sensorów lub zapisywaniu wyników pomiarów. Dzięki temu procesowi możliwe jest również tworzenie prostych systemów zapisywania danych, takich jak dziennik temperatury lub pomiarów.

## Jak to zrobić

Pisanie pliku tekstowego w Arduino może wydawać się skomplikowane, ale jest to w rzeczywistości bardzo proste. W celu utworzenia nowego pliku tekstowego, należy skorzystać z funkcji `SD.open()` i podać nazwę pliku oraz określić tryb otwarcia (np. do odczytu lub zapisu). Gdy już stworzysz plik, możesz przystąpić do pisania danych za pomocą funkcji `println()` lub `print()`, a następnie zamknąć plik za pomocą `SD.close()`.

```Arduino
#include <SPI.h>
#include <SD.h>

File dataFile; // utworzenie obiektu pliku

void setup() {
  // inicjalizacja karty SD
  SD.begin(10); 

  // utworzenie i otwarcie pliku DATA.TXT w trybie do zapisu
  dataFile = SD.open("DATA.TXT", FILE_WRITE); 
}

void loop() {
  // zapisanie danych do pliku
  dataFile.println("Temperatura: 25 stopni Celsjusza");
  // zamknięcie pliku
  dataFile.close();

  delay(1000);
}
```

Po wykonaniu tego kodu, na karcie SD pojawi się plik `DATA.TXT` zawierający wpis "Temperatura: 25 stopni Celsjusza". Dzięki zastosowaniu pętli `loop()`, możliwe jest ciągłe zapisywanie danych do pliku, co czyni go idealnym rozwiązaniem do monitorowania zmieniających się parametrów.

## Głębszy wgląd

W przypadku potrzeby dokonania więcej operacji na plikach, można skorzystać z funkcji `SD.open()` i podać dodatkowe parametry, takie jak `O_READ` (przeczytanie pliku) lub `O_CREAT` (utworzenie nowego pliku, jeśli nie istnieje). W celu zamiany liczby na tekst lub odwrotnie, można wykorzystać funkcje `itoa()` lub `atol()`, a także użyć funkcji `write()` do zapisywania pojedynczych znaków.

## Zobacz również

- [Biblioteka SD do Arduino](https://www.arduino.cc/en/Reference/SD)
- [Przykładowy projekt zapisywania danych na karcie SD](https://randomnerdtutorials.com/guide-to-sd-card-module-with-arduino/)
- [Poradnik o pamięci SD na Arduino](https://diyi0t.com/sd-card-module-arduin/)

Pisanie pliku tekstowego w Arduino może mieć wiele zastosowań i być przydatne w wielu projektach. Dzięki temu procesowi można łatwo przechowywać i analizować dane, co czyni go nieodłączną częścią programowania w Arduino. Warto poświęcić trochę czasu na zapoznanie się z tym procesem, aby w przyszłości bez problemu wykorzystywać go w swoich projektach.