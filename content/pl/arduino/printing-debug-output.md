---
title:                "Wydrukowanie wyjścia z debugowania"
html_title:           "Arduino: Wydrukowanie wyjścia z debugowania"
simple_title:         "Wydrukowanie wyjścia z debugowania"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Drukowanie informacji debugowania jest ważnym narzędziem dla programistów Arduino. Pozwala na sprawdzenie poprawności wykonywania programu oraz na łatwe znalezienie błędów i ich rozwiązanie.

## Jak to zrobić

Arduino posiada funkcję `Serial.print()`, która pozwala na wypisanie wartości na konsoli, która jest podłączona do płytki. Możemy jej użyć w prosty sposób, podając w nawiasie wartość lub zmienną, którą chcemy wydrukować. Przykład:

```Arduino
int x = 5;
Serial.print(x); // Wyświetli wartość zmiennej x, czyli 5
```

Możemy również wypisać tekst, używając funkcji `Serial.println()`, która automatycznie przechodzi do nowej linii po wypisaniu wartości. Przykład:

```Arduino
int y = 10;
Serial.print("Wartość y = "); // Wyświetli napis "Wartość y = "
Serial.println(y); // Wyświetli wartość zmiennej y, czyli 10 w nowej linii
```

Możemy również wyświetlić dwa lub więcej wartości, używając funkcji `Serial.print()` kilka razy lub `Serial.println()` z połączonym tekstem i zmiennymi. Przykład:

```Arduino
float a = 1.4;
float b = 2.6;
Serial.println("Suma a + b = " + String(a + b)); // Wyświetli tekst "Suma a + b = 4.0"
```

Możemy także skorzystać z funkcji `Serial.write()`, która przyjmuje dane w postaci bajtowej. Jest to przydatne, gdy chcemy przesłać dane na inny serwer lub urządzenie. Przykład:

```Arduino
char c[20] = "Arduino";
Serial.write(c, 7); // Wyświetli pierwsze 7 znaków ciągu znaków z tablicy c, czyli "Arduino"
```

## Deep Dive

Funkcje `Serial.print()`, `Serial.println()` oraz `Serial.write()` pozwolą nam na wyświetlanie wartości różnego typu: liczbowych, tekstowych oraz bajtowych. Możemy także kontrolować sposób wyświetlania za pomocą specjalnych znaków, takich jak `newline` lub `tab`. Przykład:

```Arduino
Serial.print("Wartość z = " + String(z) + "\t"); // Wyświetli wartość zmiennej z, a następnie dodatkową spację
Serial.print("Wartość m = " + String(m) + "\n"); // Wyświetli wartość zmiennej m w nowej linii
```

Dodatkowo, możemy skorzystać z funkcji `Serial.begin()` w `setup()`, aby ustawić prędkość transmisji na odpowiednią dla naszego projektu.

## Zobacz także

https://www.arduino.cc/reference/en/language/functions/communication/serial/print/

https://www.arduino.cc/reference/en/language/functions/communication/serial/println/

https://www.arduino.cc/reference/en/language/functions/communication/serial/write/