---
date: 2024-01-20 17:51:48.082105-07:00
description: "Wypisywanie informacji diagnostycznych (debug) pomaga \u015Bledzi\u0107\
  , co si\u0119 dzieje w programie. U\u017Cywamy tego, by szybko znale\u017A\u0107\
  \ i naprawi\u0107 b\u0142\u0119dy."
lastmod: 2024-02-19 22:04:54.817880
model: gpt-4-1106-preview
summary: "Wypisywanie informacji diagnostycznych (debug) pomaga \u015Bledzi\u0107\
  , co si\u0119 dzieje w programie. U\u017Cywamy tego, by szybko znale\u017A\u0107\
  \ i naprawi\u0107 b\u0142\u0119dy."
title: "Drukowanie komunikat\xF3w debugowania"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Wypisywanie informacji diagnostycznych (debug) pomaga śledzić, co się dzieje w programie. Używamy tego, by szybko znaleźć i naprawić błędy.

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
