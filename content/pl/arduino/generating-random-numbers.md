---
title:                "Arduino: Tworzenie losowych liczb"
simple_title:         "Tworzenie losowych liczb"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest niezwykle ważnym elementem programowania, ponieważ pozwala na tworzenie rozwiązań, które nie są przewidywalne. Jest nieodzownym narzędziem w przypadku tworzenia gier, symulacji oraz wielu innych zastosowań, gdzie losowość jest kluczowa.

## Jak to zrobić

Aby wygenerować losową liczbę w Arduino, należy użyć funkcji `random()` oraz podać przedział, z którego chcemy, aby liczba została wylosowana. Następnie, możemy przypisać wygenerowaną liczbę do zmiennej i wyświetlić ją w Monitorze Szeregowym za pomocą funkcji `println()`.

```Arduino
int number = random(1, 10);
Serial.println("Wylosowana liczba: " + String(number));
```

Przykładowy wynik w Monitorze Szeregowym:

```
Wylosowana liczba: 7
```

Możemy również wygenerować losowy znak, używając funkcji `random()` i podając jako przedział liczby odpowiadające poszczególnym literom w tabeli ASCII.

```Arduino
char letter = random(65, 91);
Serial.println("Wylosowany znak: " + String(letter));
```

Przykładowy wynik w Monitorze Szeregowym:

```
Wylosowany znak: S
```

## Wnikliwiej

Podczas generowania losowych liczb, warto pamiętać o kilku rzeczach. Po pierwsze, należy upewnić się, że wygenerowany przedział liczbowy jest odpowiednio duży, aby wynik był wystarczająco losowy. Po drugie, w przypadku potrzeby wygenerowania wielu liczb, warto użyć funkcji `randomSeed()` z wartością zmiennej, która będzie się zmieniała z każdym przebiegiem programu, na przykład `analogRead(A0)`.

## Zobacz również

- [Dokumentacja funkcji random() w Arduino](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Tutorial na temat generowania losowych liczb w Arduino](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)