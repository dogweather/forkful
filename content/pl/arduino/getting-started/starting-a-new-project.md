---
title:                "Rozpoczynanie nowego projektu"
aliases:
- /pl/arduino/starting-a-new-project.md
date:                  2024-01-20T18:03:04.662709-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rozpoczynanie nowego projektu"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Czym jest i dlaczego?)
Zaczynanie nowego projektu to stworzenie miejsca na kod, który później zamieni się w działający program. Programiści robią to, by przekształcić pomysły w konkretne elektroniczne rozwiązania.

## How to:
```Arduino
void setup() {
  // Tutaj inicjalizujesz pin, komunikację, itp.
  Serial.begin(9600); // Rozpocznij komunikację szeregową
}

void loop() {
  // Twój główny kod, który będzie się powtarzał
  Serial.println("Witaj, świecie!"); // Wyświetl tekst w monitorze szeregowym
  delay(1000); // Odczekaj sekundę
}
```

Sample output (Przykładowe wyjście):
```
Witaj, świecie!
Witaj, świecie!
Witaj, świecie!
...
```

## Deep Dive
Zanim Arduino UNO otworzyło drogę prostego programowania dla entuzjastów, był czas, kiedy tworzenie elektroniki było zdominowane przez specjalistów. W 2005 roku, grupa projektowa z Italii stworzyła Arduino, by uczynić elektronikę dostępną dla wszystkich. Alternatywy, takie jak Raspberry Pi czy ESP8266, też pozwalają na realizowanie projektów, ale Arduino wciąż wygrywa prostotą. Kiedy tworzysz nowy projekt, pamiętaj, że setup() uruchamia się jeden raz, a loop() działa w nieskończoność – to serce twojego programu.

## See Also
- [Arduino Reference](https://www.arduino.cc/reference/en/)
- [Arduino Getting Started Guide](https://www.arduino.cc/en/Guide)
- [Arduino Project Hub](https://create.arduino.cc/projecthub)
