---
date: 2024-01-20 17:51:48.082105-07:00
description: "How to: (Jak to zrobi\u0107:) Wynik w monitorze portu szeregowego."
lastmod: '2024-04-05T21:53:37.098513-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Wynik w monitorze portu szeregowego."
title: "Drukowanie komunikat\xF3w debugowania"
weight: 33
---

## How to: (Jak to zrobić:)
```Arduino
void setup() {
  Serial.begin(9600); // Start Serial Communication
}

void loop() {
  Serial.println("Hello, Debug!");
  delay(1000); // Wait for a second
}
```
Wynik w monitorze portu szeregowego:
```
Hello, Debug!
Hello, Debug!
...
```

## Deep Dive (Dogłębna analiza)
Wypisywanie informacji diagnostycznych nie zawsze było takie łatwe jak dziś. W starych mikrokontrolerach wymagało to sprytnych rozwiązań, jak migające diody. Alternatywą dla Serial jest używanie zewnętrznych wyświetlaczy lub nawet wysłanie danych do sieci. W Arduino informacje diagnostyczne są wypisywane przez port szeregowy, który jest połączony z USB, pozwalając komunikację z komputerem.

## See Also (Zobacz także)
- Dokumentacja Arduino `Serial`: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Jak użyć monitora portu szeregowego: https://www.arduino.cc/en/Guide/Environment#serial-monitor
- Wprowadzenie do debugowania w Arduino: https://learn.adafruit.com/introduction-to-the-arduino-ide/serial-monitor
