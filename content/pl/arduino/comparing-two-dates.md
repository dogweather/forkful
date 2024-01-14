---
title:    "Arduino: Porównywanie dwóch dat"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Funkcja porównywania dat jest bardzo przydatna w programowaniu, ponieważ pozwala na dokładne określenie odstępów czasowych między wydarzeniami. Można ją wykorzystać do tworzenia prostych algorytmów, które są niezbędne w wielu projektach z wykorzystaniem Arduino.

## Jak to zrobić

Aby porównać dwie daty w Arduino, należy użyć funkcji `compareDates`. Przyjmuje ona dwa argumenty: pierwszym jest pierwsza data do porównania, a drugim druga data. Następnie wywołuje się tę funkcję i przechowuje jej wynik w zmiennej. Na przykład:

```Arduino 
int firstDate = "2020-01-05";
int secondDate = "2020-02-10";

int comparison = compareDates(firstDate, secondDate);

Serial.print("Wynik porównania: "); 
```

W powyższym przykładzie, wynik porównania będzie wynosił `-36`, co oznacza, że od pierwszej daty minęło 36 dni więcej w porównaniu z drugą datą.

## Pełny zanurzenie

Funkcja `compareDates` nie tylko porównuje daty, ale również uwzględnia różnice w miesiącach i latach. Dzięki temu można dokładnie określić odstępy czasu między dwoma datami. Jeśli pierwsza data jest wcześniejsza niż druga, wynik porównania będzie ujemny. Jeśli druga data jest wcześniejsza, wynik będzie dodatni.

## Zobacz także

- Dokumentacja Arduino dotycząca porównywania dat: https://www.arduino.cc/reference/en/language/functions/time/compare/
- Przydatne tutorial i projekty z wykorzystaniem porównywania dat na Arduino: https://randomnerdtutorials.com/arduino-compare-dates-timeunit-library/