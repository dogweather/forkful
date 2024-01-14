---
title:    "Arduino: Generowanie losowych liczb"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Jedną z ważnych rzeczy w programowaniu jest możliwość generowania liczb losowych. Dzięki temu możemy stworzyć bardziej złożone i ciekawe projekty na naszych Arduino. W tym artykule pokażemy, jak można to zrobić w prosty sposób.

## Jak to zrobić

Generowanie liczb losowych na Arduino jest możliwe dzięki użyciu funkcji `random()`. Możemy jej użyć w dwóch sposobach: do wygenerowania pojedynczej losowej liczby lub do wygenerowania całego ciągu liczb.

### Generowanie pojedynczej losowej liczby

Aby wygenerować pojedynczą losową liczbę, należy użyć funkcji `random()` z podaniem zakresu, czyli najmniejszej i największej możliwej liczby. Na przykład, jeśli chcemy wygenerować liczbę z przedziału od 0 do 100, użyjemy kodu:

```Arduino
int randomNumber = random(0, 100);
```

Wartość ta zostanie zapisana do zmiennej `randomNumber` i możemy jej później użyć w naszym projekcie.

### Generowanie ciągu liczb losowych

Jeśli chcemy wygenerować więcej niż jedną liczbę, użyjemy pętli `for` oraz funkcji `random()`. Przykładowy kod może wyglądać tak:

```Arduino
for (int i = 0; i < 10; i++) { 
    int randomNumber = random(0, 100);
    Serial.println(randomNumber); // wypisujemy wygenerowaną liczbę w monitorze szeregowym
}
```

W powyższym przykładzie wygenerujemy 10 liczb z przedziału od 0 do 100 i wypiszemy je w monitorze szeregowym.

## Pogłębiona analiza

Arduino korzysta z generatora liczb pseudolosowych, czyli funkcji, która na podstawie pewnej wartości startowej (zwanej również ziarnem) generuje ciąg liczb o charakterystykach przypadkowych. W przypadku Arduino, ziarno jest generowane na podstawie zmiennej czasu, dzięki czemu każde uruchomienie układu będzie generować inne liczby.

Ważną rzeczą jest również ustawienie ziarna za każdym razem, gdy uruchamiamy generator liczb. W przypadku Arduino, robi się to automatycznie, ale w innych językach programowania może być konieczne ustawienie ziarna ręcznie.

## Zobacz także

- Dokumentacja funkcji `random()` na stronie Arduino: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Jak wykorzystać liczby losowe w swoim projekcie na Arduino: https://create.arduino.cc/projecthub/Arduino_Genuino/generate-random-numbers-with-arduino-d27d3c
- Wytłumaczenie działania generatora liczb pseudolosowych: https://pl.khanacademy.org/computing/computer-science/cryptography/crypt/v/randomness-and-non-uniformity-in-random-number-generation