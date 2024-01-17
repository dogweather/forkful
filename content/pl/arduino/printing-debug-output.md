---
title:                "Wyświetlanie wyników debugowania"
html_title:           "Arduino: Wyświetlanie wyników debugowania"
simple_title:         "Wyświetlanie wyników debugowania"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# Co i dlaczego?

Drukowanie informacji debugowania jest procesem, w którym programista wypisuje szczegóły dotyczące działania swojego kodu w celu ułatwienia analizy i naprawy ewentualnych błędów. Jest to niezbędne narzędzie dla programistów, ponieważ pomaga w szybkim rozwiązywaniu problemów i poprawnym funkcjonowaniu aplikacji.

# Jak to zrobić?

Możesz użyć funkcji `Serial.print()` lub `Serial.println()` w Arduino, aby wypisać tekst lub wartość zmiennej. Możesz także ustawić odpowiednią prędkość transmisji **Serial.begin()** w funkcji `setup()`. Przykładowy kod wygląda tak.

```Arduino
Serial.begin(9600);
int x = 5;
Serial.print("Wartość x wynosi: ");
Serial.println(x);
```

# Głębsza medytacja

Drukowanie informacji debugowania w programowaniu jest praktyką, która ma swoje korzenie w pisaniu aplikacji na komputery. Alternatywą dla tej metody jest korzystanie z narzędzi do debugowania dostępnych w środowisku Arduino lub wykorzystanie biblioteki do obsługi portu szeregowego. Jeśli potrzebujesz bardziej zaawansowanych funkcji, takich jak filtrowanie lub zapis do pliku, możesz skorzystać z biblioteki software serial.

# Zobacz też

* [Arduino - Dokumentacja o drukowaniu informacji debugowania](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
* [Inne metody debugowania w Arduino](https://www.arduino.cc/en/Tutorial/Debugging)