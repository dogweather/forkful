---
date: 2024-01-26 01:16:44.467176-07:00
description: "Jak to zrobi\u0107: Za\u0142\xF3\u017Cmy, \u017Ce masz na swoim Arduino\
  \ funkcj\u0119, kt\xF3ra robi zbyt wiele, na przyk\u0142ad tak\u0105."
lastmod: '2024-03-13T22:44:35.679649-06:00'
model: gpt-4-0125-preview
summary: "Za\u0142\xF3\u017Cmy, \u017Ce masz na swoim Arduino funkcj\u0119, kt\xF3\
  ra robi zbyt wiele, na przyk\u0142ad tak\u0105."
title: Refaktoryzacja
weight: 19
---

## Jak to zrobić:
Załóżmy, że masz na swoim Arduino funkcję, która robi zbyt wiele, na przykład taką:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Funkcja robiąca zbyt wiele
  handleEverything();
}

void handleEverything() {
  // Odczyt danych z czujnika
  int sensorValue = analogRead(A0);
  // Przetwarzanie danych z czujnika
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // Drukowanie danych z czujnika
  Serial.println(sensorValue);
  delay(500);
}
```

Refaktoryzacja może polegać na podzieleniu `handleEverything()` na mniejsze, bardziej skupione funkcje:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

Po refaktoryzacji funkcja `loop()` jest bardziej czytelna, a każde zadanie jest obsługiwane przez dedykowaną funkcję, co sprawia, że kod jest łatwiejszy do zarządzania.

## Głębsze spojrzenie
Historycznie, refaktoryzacja zyskała na popularności wraz z rozwojem metodyk Agile i Test-Driven Development (TDD), które opierają się na ciągłym ulepszaniu kodu w celu dostosowania się do zmieniających się wymagań. Istnieją różne narzędzia i strategie refaktoryzacji - takie jak technika "Extract Method", której użyliśmy w naszym przykładzie z Arduino. Jest to niezbędne, gdy przechodzisz od szybkiego prototypu do stabilnego projektu, w którym czytelność i łatwość utrzymania kodu stają się kluczowe.

Podczas refaktoryzacji ważne jest, aby mieć dobry zestaw testów, aby upewnić się, że zmiany nie wprowadziły żadnych błędów. W świecie Arduino, automatyczne testowanie nie zawsze jest proste ze względu na zależności sprzętowe, ale nadal można używać testów jednostkowych dla części zawierających czystą logikę lub korzystać z symulatorów.

Alternatywami dla ręcznej refaktoryzacji są dedykowane narzędzia do refaktoryzacji, które automatyzują identyfikację "zapachów" kodu i sugerują zmiany. Jednak te narzędzia często nie uwzględniają subtelności kodu mikrokontrolera i mogą nie być dostępne w środowisku rozwojowym Arduino.

Ostatecznie, refaktoryzacja to sztuka równoważenia między poprawą wewnętrznej struktury kodu a ryzykiem wprowadzenia defektów. Wymaga to od Ciebie myślenia o szczegółach implementacji, takich jak zużycie pamięci i czas procesora, zwłaszcza ze względu na ograniczone zasoby mikrokontrolerów.

## Zobacz także
Możesz pogłębić wiedzę na temat refaktoryzacji dzięki kluczowej książce Martina Fowlera *Refaktoryzacja: Ulepszanie projektu istniejącego kodu*. Aby przyjrzeć się bliżej praktykom specyficznym dla Arduino, sprawdź fora i społeczności programistów Arduino:

- [Forum Arduino - Pytania dotyczące programowania](https://forum.arduino.cc/index.php?board=4.0)
- [Refactoring Guru](https://refactoring.guru/refactoring)

Pamiętaj, że celem jest czysty, zrozumiały kod, za który przyszły Ty i inni będą Ci wdzięczni. Kontynuuj hakerstwo i dbaj o porządek!
