---
title:    "Arduino: Sprawdzanie, czy istnieje katalog"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego
Jeśli chcesz mieć pewność, że dany folder istnieje przed wykonaniem pewnych działań, istnieją specjalne funkcje w języku Arduino, które pozwolą Ci to sprawdzić. Dzięki temu unikniesz niepożądanych błędów i zapewnisz poprawne działanie Twojego kodu.

## Jak to zrobić
Sprawdzenie istnienia folderu jest bardzo proste w języku Arduino. Wystarczy użyć wbudowanej funkcji `fileExists()`, która przyjmuje jako argument ścieżkę do folderu. Poniżej znajduje się przykładowy kod, który pokaże Ci, jak to zrobić:
```Arduino
#include <SPI.h>
#include <SD.h>
File myFile;

void setup() {
  Serial.begin(9600);
  if (SD.begin(10)) {
    if (SD.exists("/MojaPiosenka")) {
      Serial.println("Folder istnieje!");
    } else {
      Serial.println("Folder nie istnieje!");
    }
  } else {
    Serial.println("Błąd inicjalizacji karty SD!!");
  }
}

void loop() {
}
```
Po wgraniu tego kodu do płytki Arduino i otwarciu monitora szeregowego, powinno pojawić się odpowiednie powiadomienie informujące o istnieniu lub nieistnieniu folderu.

## Deep Dive
Podczas tworzenia projektów z wykorzystaniem Arduino często mamy do czynienia z zapisywaniem lub odczytywaniem danych z karty SD. W takich przypadkach ważne jest, aby przed wywołaniem odpowiednich funkcji, uprzednio sprawdzić czy dany folder istnieje. W przeciwnym przypadku narażamy się na błędy, a nawet utratę danych.

Funkcja `fileExists()` działa na podobnej zasadzie jak funkcja `SD.exists()`, jednak pozwala na sprawdzenie istnienia nie tylko plików, ale również folderów. Możemy również użyć tej funkcji, aby sprawdzić czy istnieje określony plik w danym folderze. Należy wówczas podać pełną ścieżkę do pliku, np. `/MojaPiosenka/Piosenka.mp3`.

## Zobacz też
* [Dokumentacja funkcji `fileExists()`](https://www.arduino.cc/reference/en/libraries/sd/fileexists/)
* [Dokumentacja funkcji `SD.exists()`](https://www.arduino.cc/reference/en/libraries/sd/exists/)