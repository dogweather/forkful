---
date: 2024-01-20 18:03:04.662709-07:00
description: "How to: Sample output (Przyk\u0142adowe wyj\u015Bcie)."
lastmod: '2024-04-05T21:53:37.097772-06:00'
model: gpt-4-1106-preview
summary: "Sample output (Przyk\u0142adowe wyj\u015Bcie)."
title: Rozpoczynanie nowego projektu
weight: 1
---

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
