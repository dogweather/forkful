---
title:                "Pisanie do standardowego wyjścia błędów"
html_title:           "Arduino: Pisanie do standardowego wyjścia błędów"
simple_title:         "Pisanie do standardowego wyjścia błędów"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu jest ważnym narzędziem w programowaniu, ponieważ pozwala nam na wyświetlenie komunikatów o błędach i ostrzeżeń w czasie wykonywania programu. Jest to szczególnie przydatne podczas debugowania kodu i znajdowania potencjalnych problemów.

## Jak to zrobić

Używanie Arduino do pisania do standardowego błędu jest bardzo proste. Wystarczy użyć funkcji ```Serial.println()``` i podać wiadomość lub wartość, którą chcemy wyświetlić. Poniższy przykład wyświetla komunikat "Błąd: wartość czujnika jest za niska!" w czasie wykonywania programu:

```
Arduino void setup() {
  // kod inicjalizacyjny
}

void loop() {
  // kod wykonywany w pętli
  if (value < 10) {
    Serial.println("Błąd: wartość czujnika jest za niska!");
  }
}
```

W takim przypadku, jeśli wartość przekracza 10, nic się nie wyświetli. W przeciwnym razie, komunikat zostanie wyświetlony w konsoli Arduino.

## Głębszy zanurzanie

Podczas pisania do standardowego błędu istnieje kilka ważnych rzeczy, których należy być świadomym. Pierwszą z nich jest ustawienie prędkości transmisji (baud rate) w funkcji ```Serial.begin()```. Domyślnie jest to 9600 bitów na sekundę, ale można to zmienić na inną wartość, dopasowując ją do swoich potrzeb. Należy również pamiętać, aby zawsze zamknąć konsolę monitora serialowego przed wręczeniem finalnego projektu.

## Zobacz również

Jeśli potrzebujesz dalszej pomocy w pisaniu do standardowego błędu w Arduino, zapoznaj się z poniższymi linkami:

- Dokumentacja funkcji ```Serial.println()```: https://www.arduino.cc/reference/en/language/functions/communication/serial/println/
- Tutoriał na temat debugowania kodu w Arduino: https://learn.sparkfun.com/tutorials/debugging-an-arduino-sketch/all
- Forum wsparcia dla Arduino: https://forum.arduino.cc/