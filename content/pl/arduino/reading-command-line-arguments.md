---
title:                "Arduino: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy znasz ten moment, kiedy chcesz zmienić ustawienia swojego projektu Arduino, ale musisz przeprogramować cały kod? To czasochłonne i może być frustrujące. Ale czy wiesz, że istnieje sposób aby zmienić ustawienia bez zmiany całego kodu? Taki sposób to czytanie argumentów wiersza poleceń! W tym artykule dowiesz się, jak to zrobić.

## Jak To Zrobić
```Arduino

void setup()
{
  //rozpocznij komunikację z portem szeregowym
  Serial.begin(9600);

  //ustaw domyślny czas trwania pomiarów na 1000 ms
  int czasPomiaru = 1000;

  //jeśli zostaną podane argumenty wiersza poleceń, zmień czas pomiaru na podany
  if (Serial.available() > 0)
  {
    czasPomiaru = Serial.parseInt(); //odczytaj czas pomiaru z argumentów
    Serial.print("Nowy czas pomiaru: ");
    Serial.println(czasPomiaru);
  }

  //kod mierzący wartość wyjścia pinu A0
  int wartoscCzujnika = analogRead(A0);

  //wyświetl pomiar
  Serial.print("Wartość czujnika: ");
  Serial.println(wartoscCzujnika);

  //przerwa 1000 ms przed kolejnym pomiarem
  delay(czasPomiaru);
}

void loop() {}

```

Przetestuj ten kod w swoim projekcie Arduino. Uruchom monitor portu szeregowego i wprowadź wartość w milisekundach jako argument wiersza poleceń (np. ```2000```). Zauważ, jak zmienia się czas trwania pomiarów.

## Deep Dive
Teraz, gdy już wiesz jak czytać argumenty wiersza poleceń, możesz dowolnie zmieniać ustawienia swojego projektu. Możesz też wykorzystać tę funkcję dla bardziej zaawansowanych zastosowań, takich jak zmiana trybu pracy na podstawie argumentów lub przesyłanie danych z komputera do Arduino za pomocą argumentów.

## Zobacz też
- [Arduino Serial Monitor](https://www.arduino.cc/reference/en/language/functions/communication/serial/monitor/)
- [Arduino Command-Line Interface](https://www.arduino.cc/en/Main/SoftwareCMDLine)
- [Serial.parseInt()](https://www.arduino.cc/reference/en/language/functions/communications/serial/parseint/)