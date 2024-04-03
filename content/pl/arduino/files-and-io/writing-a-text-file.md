---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:04.902029-07:00
description: "Zapisywanie pliku tekstowego w Arduino polega na zapisywaniu danych\
  \ na karcie SD lub podobnym module pami\u0119ci, cz\u0119sto w celach rejestracji\
  \ danych.\u2026"
lastmod: '2024-03-13T22:44:35.689789-06:00'
model: gpt-4-0125-preview
summary: "Zapisywanie pliku tekstowego w Arduino polega na zapisywaniu danych na karcie\
  \ SD lub podobnym module pami\u0119ci, cz\u0119sto w celach rejestracji danych."
title: Pisanie pliku tekstowego
weight: 24
---

## Jak to zrobić:
Aby zapisać plik tekstowy na karcie SD za pomocą Arduino, należy najpierw dołączyć bibliotekę `SD.h`, która zapewnia niezbędne funkcje do interakcji z kartami SD. Upewnij się, że Twoja płyta Arduino jest podłączona do modułu karty SD.

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // Inicjalizacja komunikacji szeregowej z prędkością 9600 bitów na sekundę:
  Serial.begin(9600);
  
  // Sprawdzenie inicjalizacji karty SD
  if (!SD.begin(4)) {
    Serial.println("Inicjalizacja nie powiodła się!");
    return;
  }
  Serial.println("Inicjalizacja zakończona.");
  
  // Otwórz plik. Zauważ, że na raz można otworzyć tylko jeden plik,
  // więc musisz zamknąć ten zanim otworzysz inny.
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // Jeśli plik został pomyślnie otwarty, zapisz do niego:
  if (myFile) {
    Serial.print("Zapisywanie do test.txt...");
    myFile.println("Testowanie zapisu pliku tekstowego.");
    // Zamknij plik:
    myFile.close();
    Serial.println("zakończono.");
  } else {
    // Jeśli plik się nie otworzył, wyświetl błąd:
    Serial.println("Błąd przy otwieraniu test.txt");
  }
}

void loop() {
  // Nic się nie dzieje po ustawieniu
}
```

### Przykładowy wynik:
Gdy uruchomisz ten kod, monitor szeregowy środowiska Arduino IDE wyświetli:
```
Inicjalizacja zakończona.
Zapisywanie do test.txt...zakończono.
```
Aby sprawdzić, czy dane zostały poprawnie zapisane, możesz usunąć kartę SD z Arduino, włożyć ją do komputera i otworzyć plik `test.txt`, aby zobaczyć wiadomość "Testowanie zapisu pliku tekstowego."

Dla projektów wymagających bardziej zaawansowanych operacji na plikach lub przetwarzania, rozważ eksplorację dodatkowych bibliotek lub pisanie własnych funkcji dostosowanych do swoich specyficznych potrzeb.
