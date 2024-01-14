---
title:    "Arduino: Pisanie do standardowego błędu"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego pisać do standardowego błędu w Arduino?

Pisanie do standardowego błędu jest ważnym aspektem programowania w Arduino, ponieważ pozwala nam na wyświetlanie informacji diagnostycznych na monitorze szeregowym. To przydatne narzędzie podczas debugowania kodu i szukania błędów w aplikacji. Bez wyświetlania informacji o błędach, trudno jest zlokalizować i naprawić problemy w naszym programie.

## Jak to zrobić?

Pisanie do standardowego błędu w Arduino jest bardzo proste. Wystarczy użyć funkcji ```Serial.println()``` lub ```Serial.print()```, podając jako parametr tekst lub wartość, którą chcemy wyświetlić na monitorze szeregowym. Poniższy przykład przedstawia użycie tej funkcji:

```
void setup() {
  Serial.begin(9600); // ustawienie prędkości transmisji danych
}

void loop() {
  int sensorValue = analogRead(A0); // odczytanie wartości z czujnika analogowego
  Serial.println("Aktualna wartość czujnika to: " + String(sensorValue)); // wyświetlenie tekstu i wartości na monitorze szeregowym
  delay(1000); // opóźnienie o 1 sekundę
}
```

Po wgraniu tego kodu do płytki Arduino i uruchomieniu monitora szeregowego (9600 bps), będziemy widzieć na wyjściu informacje o wartości odczytanej z czujnika. Dzięki temu możemy na bieżąco monitorować działanie naszej aplikacji.

## Głębsza analiza

Pisanie do standardowego błędu w Arduino jest przydatne nie tylko do wyświetlania wartości zmiennych, ale także do informowania nas o przebiegu programu, wykrywanych błędach czy ewentualnych ostrzeżeniach. W tym celu możemy użyć funkcji ```Serial.print()``` do wyświetlania krótkich komunikatów oraz funkcji ```Serial.println()``` do wyświetlania dłuższych wiadomości. Dodatkowo, możemy użyć funkcji ```Serial.begin()``` do ustawienia prędkości transmisji danych, tak aby zgadzała się z ustawieniami monitora szeregowego.

## Zobacz też 

- [Official Arduino Serial Library Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Tutorial: Arduino - Serial Communication](https://www.arduino.cc/en/Tutorial/SerialCommunication)
- [How to Use and Understand the Arduino Serial Monitor](https://www.dummies.com/computers/arduino/how-to-use-and-understand-the-arduino-serial-monitor/)