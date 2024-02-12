---
title:                "Organizacja kodu w funkcje"
aliases:
- /pl/arduino/organizing-code-into-functions.md
date:                  2024-01-26T01:08:39.068763-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizacja kodu w funkcje"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Organizacja kodu w funkcje oznacza podzielenie kodu na wielokrotnie używalne bloki, z których każdy wykonuje określone zadanie. Programiści robią to, aby kod był łatwiejszy do odczytania, debugowania i ponownego użycia. To jak sortowanie klocków Lego do pojemników - oszczędza to przeszukiwania chaotycznego stosu za każdym razem, gdy chcesz coś zbudować.

## Jak to zrobić:
Wyobraź sobie, że chcesz mrugać diodą LED. Bez funkcji, twoja `pętla` to bałagan. Z funkcjami jest uporządkowana. Oto jak:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // Mruganie diodą LED co 500ms
}

// Funkcja mrugania diodą LED
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

Przykładowy efekt: Twoja dioda LED mruga wesoło, a cel kodu jest jasny na pierwszy rzut oka.

## Zanurzenie się głębiej
Przed funkcjami programowanie było jak linearna podróż drogowa; widziałeś każdą dziurę od początku do końca. Po funkcjach jest to bardziej jak przeskakiwanie lotami - przechodzisz do ważnych części. Historycznie, podprogramy (wczesne funkcje) były rewolucją w programowaniu, pozwalającym programistom unikać powtarzania się – to zasada DRY, Don’t Repeat Yourself (Nie Powtarzaj Się). Alternatywy dla funkcji mogą obejmować makra lub używanie klas do programowania zorientowanego obiektowo (OOP). Istota rzeczy? Kiedy definiujesz funkcję, dajesz kompilatorowi plan wykonania zadania. Przy Arduino często definiujesz funkcje typu void, które działają jak proste komendy dla mikrokontrolera, ale funkcje mogą również zwracać wartości, co czyni je bardziej wszechstronnymi.

## Zobacz również
Po więcej informacji na temat funkcji, przejrzyj te źródła:

- Oficjalna dokumentacja funkcji Arduino: https://www.arduino.cc/reference/en/language/functions/
- Dowiedz się więcej o zasadzie DRY: https://pl.wikipedia.org/wiki/Nie_powtarzaj_się
- Odświeżenie wiedzy na temat historii podprogramów: https://pl.wikipedia.org/wiki/Podprogram
