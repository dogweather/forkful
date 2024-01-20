---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Czytanie argumentów linii poleceń to metoda przyjmowania inputu od użytkownika. Programiści korzystają z tego do dostosowywania programów na podstawie desygnacji użytkownika.

## Jak to zrobić:
Arduino nie obsługuje bezpośrednio argumentów linii poleceń, ale może komunikować się z komputerem za pomocą portu szeregowego. Poniżej znajduje się przykład, który pokazuje, jak odczytać dane wysyłane do Arduino przez Serial.

```Arduino
String incomingData;

void setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available())  {
    incomingData = Serial.readStringUntil('\n');
    Serial.println("Odebrane dane: " + incomingData);
  }
}
```

Jeżeli wpiszemy "Hello World" do Serial Monitor, Arduino zwróci "Odebrane dane: Hello World".

## W głąb tematu
Argumenty linii poleceń są powszechnie używane w programowaniu komputerów od lat. Arduino nie obsługuje ich bezpośrednio, ale poprzez interakcje szeregowe z komputerem, możemy odczytać dane wysłane do urządzenia. W przeciwnym razie, moglibyśmy użyć alternatywnych platform programowania mikrokontrolerów, które natywnie obsługują argumenty linii poleceń.

Gdy chodzi o szczegóły implementacji, komenda `Serial.available()` sprawdza, czy są dostępne jakiekolwiek dane do odczytania. `Serial.readStringUntil('\n')` odczytuje dane aż do napotkania znaku końca linii ('\n'). Razem ułatwiają one odczyt danych wysyłanych do Arduino przez Serial.

## Zobacz także
Oto kilka linków do pokrewnych źródeł:
- [Dokumentacja Arduino](https://www.arduino.cc/reference/pl/)
- [Serial - komunikacja UART](https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent)