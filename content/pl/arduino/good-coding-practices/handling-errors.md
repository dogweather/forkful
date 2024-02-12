---
title:                "Obsługa błędów"
aliases: - /pl/arduino/handling-errors.md
date:                  2024-01-26T00:49:28.435244-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obsługa błędów"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/handling-errors.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obsługa błędów w twoich programach wyławia te nieprzewidziane sytuacje, które próbują cię podstawić. Robisz to, aby zapobiec "stopieniu się" twojego Arduino, gdy wystąpi coś nieoczekiwanego.

## Jak to zrobić:

Załóżmy, że twoje Arduino odczytuje wartości z czujnika, który może sporadycznie wyprodukować wartości z poza zakresu. Oto jak możesz sobie z tym poradzić:

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // Wartość znajduje się w zakresie, kontynuuj przetwarzanie
  Serial.println(sensorValue);
} else {
  // Wartość znajduje się poza zakresem, obsłuż błąd
  Serial.println("Błąd: Wartość sensora poza zakresem.");
}
```
Przykładowe wyjście:
```
523
Błąd: Wartość sensora poza zakresem.
761
```

## Głębsze zanurzenie

Obsługa błędów nigdy nie była tak prosta. W początkach, programiści często ignorowali błędy, co prowadziło do obawianego "niezdefiniowanego zachowania". Wraz z ewolucją programowania ewoluowały również narzędzia — w wielu językach masz teraz wyjątki, ale w świecie Arduino nadal obowiązuje stary dobry "sprawdź to najpierw" ze względu na ograniczenia sprzętowe i korzenie w C++.

W programowaniu Arduino często widzi się instrukcje `if-else` dla obsługi błędów. Ale są alternatywy: używanie funkcji `assert`, aby zatrzymać wykonanie jeśli warunek zawiedzie, lub projektowanie zabezpieczeń w samym ustawieniu sprzętu.

Implementując obsługę błędów, zastanów się nad wpływem zatrzymania programu w porównaniu do pozwalania mu na kontynuację z domyślnym lub bezpiecznym stanem. Jest to kompromis, a właściwy wybór zależeć będzie od potencjalnych szkód wynikających z przerwania działania w porównaniu do nieprawidłowej operacji.

## Zobacz także

Poszerz swoją wiedzę o wykrywanie i obsługę błędów z tymi zasobami:

- Referencje języka Arduino: https://www.arduino.cc/reference/en/
- Bardziej dogłębne spojrzenie na obsługę błędów od Embedded Artistry: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- Obsługa błędów w C++: https://en.cppreference.com/w/cpp/error/exception

To powinno dać ci wiedzę i pewność, aby unikać pułapek błędów w twoich przygodach z Arduino.
