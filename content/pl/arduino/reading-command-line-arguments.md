---
title:    "Arduino: Odczytywanie argumentów wiersza poleceń"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Wykorzystywanie argumentów wiersza poleceń w programowaniu Arduino może być bardzo przydatne w wielu sytuacjach. Dzięki nim możesz interaktywnie wpływać na działanie swojego programu, zmieniać jego parametry lub nawet przesyłać dane z zewnętrznych urządzeń.

## Jak to zrobić

Aby odczytywać argumenty wiersza poleceń w programowaniu Arduino, wykorzystujemy funkcję `Serial.parseInt()`. Spójrz na poniższy przykład:

```Arduino
void setup() {
  Serial.begin(9600); // inicjalizacja komunikacji szeregowej z prędkością 9600 baud
}

void loop() {
  if (Serial.available()) // sprawdzamy, czy dane są dostępne w linii szeregowej
  {
    int arg = Serial.parseInt(); // odczytujemy wartość argumentu i zapisujemy ją w zmiennej arg
    Serial.println("Wczytano argument: " + String(arg)); // wyświetlamy wczytany argument
  }
}
```

W powyższym przykładzie, w momencie kiedy na linii szeregowej pojawi się jakaś wartość, zostanie ona odczytana i wyświetlona na monitorze szeregowym.

## Deep Dive

Funkcja `Serial.parseInt()` dzieli odczytaną linię na poszczególne słowa i próbuje przekonwertować je na liczby całkowite. W przypadku niepowodzenia zwracana jest wartość 0. Dzięki temu możemy sprawdzić, czy podana przez użytkownika wartość jest poprawną liczbą, a w razie potrzeby wykonać odpowiednie operacje na wczytanym argumencie.

Ważnym aspektem jest również pamiętanie o zabezpieczeniu naszego programu w przypadku niepodania argumentu. Możemy to zrobić poprzez użycie warunku `if (Serial.available())` jak w przykładzie powyżej.

## Zobacz też

- [Dokumentacja funkcji Serial.parseInt()](https://www.arduino.cc/reference/en/language/functions/communication/serial/parseint/)
- [Przykładowy projekt wykorzystujący argumenty wiersza poleceń w komunikacji między Arduino a komputerem](https://github.com/ArduinoPolska/serial-communication-with-arduino/tree/master/CommandlineArgs)