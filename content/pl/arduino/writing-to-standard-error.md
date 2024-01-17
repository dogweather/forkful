---
title:                "Pisanie do standardowego błędu"
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?

Pisząc do standardowego błędu w Arduino oznacza wysyłanie komunikatów o błędach lub ostrzeżeń do specjalnego strumienia, który jest przeznaczony do wyświetlania użycia konsoli. Programiści robią to, aby uzyskać informacje o ewentualnych problemach w swoim kodzie i pomóc w identyfikacji błędów.

## Jak to zrobić?

```Arduino
Serial.print("Błąd: Brak połączenia");
```
W powyższym przykładzie wykorzystujemy funkcję `Serial.print()` do wysłania komunikatu o błędzie "Brak połączenia" do standardowego błędu. Możemy także użyć `Serial.println()` aby dodać nową linię po komunikacie. Oba te polecenia wymagają wcześniejszego zainicjowania portu szeregowego przy użyciu polecenia `Serial.begin()`.

## Deep Dive

Pisanie do standardowego błędu jest stosowane od dawna w programowaniu, szczególnie w przypadku języków wysokiego poziomu, aby pomóc programistom w debugowaniu swojego kodu. Alternatywną metodą jest użycie debuggera, ale nie wszystkie platformy lub języki obsługują tę funkcję. W przypadku Arduino, zawarte oprogramowanie zapewnia konsolę szeregową, która umożliwia wykorzystanie funkcji pisania do standardowego błędu.

## Zobacz także

- Dokumentacja Arduino o wykorzystywaniu Serial Communication: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Poradnik o debuggowaniu w Arduino: https://learn.adafruit.com/debugging-arduino-code-with-seral-serial-begin-and-print/serial-begin