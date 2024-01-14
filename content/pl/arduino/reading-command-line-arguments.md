---
title:                "Arduino: Odczytywanie argumentów wiersza poleceń"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego
Arduino jest jedną z najpopularniejszych platform programistycznych, a jego możliwości są ogromne. Jednak, żeby w pełni wykorzystać potencjał tej platformy, ważne jest, aby poznać różne sposoby programowania. Dziś postaram się przybliżyć wam jedną z nich – czytanie argumentów wiersza poleceń.

## Jak to zrobić
Aby odczytać argumenty wiersza poleceń, musimy wykorzystać metodę ```readCommandLine()```. Przykładowy kod wyglądałby następująco:

```Arduino
String argument = readCommandLine(); // Przypisanie pierwszego argumentu do zmiennej "argument"

Serial.println(argument); // Wyświetlenie argumentu w monitorze szeregowym
```

Przykładowy output:
```
> mój-program pierwszy_argument drugi_argument
pierwszy_argument
```

W powyższym przykładzie, wykorzystaliśmy metodę ```readCommandLine()``` oraz funkcję ```Serial.println()```, aby wyświetlić pierwszy argument wiersza poleceń.

## Głębszy zanurzenie
Oczywiście, argumentów wiersza poleceń może być więcej niż jeden. W takiej sytuacji, może być przydatne wykorzystanie tablicy do przechowywania każdego argumentu. Przykładowy kod wyglądałby następująco:

```Arduino
int maxArgs = 3; // Maksymalna liczba argumentów
String args[maxArgs]; // Tablica przechowująca argumenty

for(int i = 0; i < maxArgs; i++) {
  args[i] = readCommandLine(); // Odczytanie argumentów i zapisanie ich do tablicy

  Serial.print("Argument ");
  Serial.print(i);
  Serial.print(": ");
  Serial.println(args[i]); // Wyświetlenie każdego argumentu w monitorze szeregowym
}
```

Przykładowy output:
```
> mój-program pierwszy_argument drugi_argument trzeci_argument
Argument 0: pierwszy_argument
Argument 1: drugi_argument
Argument 2: trzeci_argument
```

W ten sposób, możemy wykorzystać tablicę do przechowywania różnych argumentów, zamiast robić to ręcznie.

## Zobacz również
- [Dokumentacja Arduino o funkcji ```readCommandLine()```](https://www.arduino.cc/reference/en/language/functions/communication/serial/readcommandline/)
- [Tutorial o obsłudze argumentów wiersza poleceń w Arduino](https://learn.adafruit.com/serial-debugging-for-beginners/command-line-arguments)