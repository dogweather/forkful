---
title:                "Arduino: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Dlaczego sprawdzamy istnienie katalogu w programowaniu dla Arduino?

Sprawdzanie istnienia katalogu jest ważną częścią programowania dla Arduino, ponieważ daje nam możliwość sprawdzenia, czy dany katalog istnieje przed wykonywaniem kolejnych operacji. Jest to szczególnie przydatne przy korzystaniu z kart SD lub innych pamięci masowych, gdzie bez uprzedniego sprawdzenia można narazić się na błędy.

# Jak to zrobić?

Do sprawdzenia istnienia katalogu możemy wykorzystać funkcję `exists()` z biblioteki `SD` dla kart SD lub `SPIFFS.exists()` dla pamięci SPIFFS. Poniżej znajduje się przykładowy kod, który sprawdzi, czy istnieje katalog o nazwie "moj_katalog" i wypisze odpowiedni komunikat w zależności od wyniku:

```

#include <SD.h>

File myDir;

void setup() {

  Serial.begin(9600);
  
  // Inicjalizacja karty SD
  if (!SD.begin()) {
    Serial.println("Nie można zainicjalizować karty SD!");
    while (1);
  }

  // Przypisanie uchwytu do katalogu
  myDir = SD.open("moj_katalog");

  // Sprawdzenie istnienia katalogu i wypisanie odpowiedniego komunikatu
  if (myDir) {
    Serial.println("Katalog istnieje!");
  } else {
    Serial.println("Katalog nie istnieje!");
  }
}

void loop() {
  // Nic nie robimy w pętli
}
```

Po wgraniu tego kodu na nasze Arduino i podłączeniu karty SD, w monitorze szeregowym powinniśmy zobaczyć odpowiedni komunikat.

# Deep Dive

Sprawdzenie istnienia katalogu nie jest skomplikowaną operacją, ale może okazać się niezbędne do uniknięcia błędów podczas korzystania z pamięci masowych. Warto jednak zwrócić uwagę, że funkcja `exists()` zwraca wartość logiczną `true` lub `false` w zależności od wyniku, więc może być również wykorzystywana w warunkach do podjęcia odpowiednich działań.

# Zobacz też

- [Dokumentacja funkcji exists() dla Biblioteki SD](https://www.arduino.cc/en/Reference/SDexists)
- [Dokumentacja funkcji exists() dla Biblioteki SPIFFS](https://arduino-esp8266.readthedocs.io/en/latest/filesystem.html#exists)
- [Przykład korzystania z kart SD z Arduino](https://www.arduino.cc/en/tutorial/cardinfo)
- [Przykład korzystania z pamięci SPIFFS z Arduino](https://randomnerdtutorials.com/esp8266-nodemcu-vs-code-platformio-mockup-mern-stack/)