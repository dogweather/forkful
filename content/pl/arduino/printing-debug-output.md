---
title:                "Arduino: Wydrukowanie wyników debugowania"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie jest procesem często wymagającym wielu iteracji i testów, aby osiągnąć pożądane efekty. Jednym ze sposobów ułatwiających ten proces jest drukowanie debug outputów, czyli informacji wyświetlanych w konsoli, które pozwalają nam śledzić i analizować działanie naszego kodu. W tym artykule dowiesz się, dlaczego warto wykorzystywać drukowanie debug outputów w programowaniu na Arduino.

## Jak To Zrobić

Drukowanie debug outputów w programowaniu na Arduino jest bardzo proste i wymaga zaledwie kilku linii kodu. Najpierw musimy zdefiniować port szeregowy za pomocą funkcji `Serial.begin()`, która odpowiada za komunikację między Arduino a komputerem. Następnie możemy wykorzystać funkcję `Serial.print()` lub `Serial.println()` do wyświetlania informacji w konsoli. Przykładowy kod wyglądałby następująco:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.print("Witaj, świecie!");
  delay(1000);
}
```

W powyższym przykładzie, co 1000 milisekund zostanie wyświetlony tekst `Witaj, świecie!` w konsoli. Możemy także wyświetlać wartości zmiennych, co jest bardzo przydatne podczas debugowania naszego kodu. Przykładowo:

```Arduino
int liczba = 10;

void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.print("Wartość zmiennej liczba: ");
  Serial.println(liczba);
  delay(1000);
}
```

W konsoli będzie wyświetlany tekst `Wartość zmiennej liczba: 10` co 1000 milisekund. Dzięki temu możemy monitorować zmiany wartości naszych zmiennych i łatwiej identyfikować problemy w kodzie.

## Głębsza Analiza

Drukowanie debug outputów pozwala nam w prosty sposób analizować działanie naszego kodu, a także pomaga wychwycić potencjalne błędy. Możemy wyświetlać informacje o wykonywanych pętlach, wartościach zmiennych, czy aktualnym stanie czujników. Dzięki temu możemy śledzić, co dzieje się w naszym programie i zmieniać go w razie potrzeby.

Warto także pamiętać, że drukowanie debug outputów może wpływać na wydajność naszego programu. Dlatego po zakończonej analizie i debugowaniu, należy wyłączyć lub usunąć niepotrzebne wydruki, aby nasz kod działał szybciej.

## Zobacz Również

* [Dokumentacja Arduino Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
* [Tutorial: Jak wykorzystywać drukowanie debug outputów w Arduino IDE](https://www.arduino.cc/en/Tutorial/SerialBegin)
* [Tutorial: Wykorzystywanie drukowania debug outputów zewnętrznym programem](https://learn.sparkfun.com/tutorials/serial-debugging-with-the-arduino/using-a-serial-terminal)

Dzięki drukowaniu debug outputów możemy skuteczniej analizować nasz kod i ułatwić sobie proces programowania na Arduino. Pamiętaj jednak, że najlepszym sposobem na unikanie błędów jest pisanie czytelnego i zrozumiałego kodu.