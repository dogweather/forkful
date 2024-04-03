---
date: 2024-01-27 20:32:36.439018-07:00
description: "Jak to zrobi\u0107: Arduino oferuje proste funkcje do generowania losowych\
  \ liczb: `randomSeed()` i `random()`. Na pocz\u0105tek zainicjuj generator liczb\
  \ losowych,\u2026"
lastmod: '2024-03-13T22:44:35.668283-06:00'
model: gpt-4-0125-preview
summary: Arduino oferuje proste funkcje do generowania losowych liczb.
title: Generowanie liczb losowych
weight: 12
---

## Jak to zrobić:
Arduino oferuje proste funkcje do generowania losowych liczb: `randomSeed()` i `random()`. Na początek zainicjuj generator liczb losowych, aby zapewnić różne sekwencje liczb za każdym uruchomieniem programu. Często stosowanym podejściem jest zasianie za pomocą odczytu analogowego z niepodłączonego pinu.

```Arduino
void setup() {
  Serial.begin(9600);
  // Zainicjowanie ziarna losowości
  randomSeed(analogRead(0));
}

void loop() {
  // Wygeneruj losową liczbę między 0 a 99
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // Opóźnienie na sekundę dla czytelności wyników
}
```

Powyższy program inicjuje generator liczb losowych w funkcji `setup()` i generuje nową liczbę między 0 a 99 w każdej iteracji pętli, wyprowadzając liczbę do Monitora Szeregowego.

Przykładowe wyjście:
```
42
17
93
...
```

## Dogłębna analiza
Funkcja `random()` w Arduino pod maską wykorzystuje generator pseudo-losowych liczb (PRNG), który podąża za określoną, deterministyczną sekwencją, ale wygląda na statystycznie losowy. Początkowa wartość, czyli ziarno sekwencji, mocno wpływa na jej nieprzewidywalność, stąd powszechne użycie `randomSeed()` z dość losowym wejściem jako punktu wyjścia. Ważne jest, aby zauważyć, że losowość generowana przez Arduino jest wystarczająca dla większości projektów hobbystycznych, ale może nie spełniać kryteriów dla aplikacji o wysokim poziomie bezpieczeństwa ze względu na jej przewidywalność w czasie. Do celów kryptograficznych zaleca się przyjrzeć bardziej zaawansowanym algorytmom i hardware'owym generatorom losowych liczb (HRNG), które mogą zapewnić prawdziwą losowość, wykorzystując procesy fizyczne.
