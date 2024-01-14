---
title:                "Haskell: Znajdowanie długości ciągu znaków"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego?

Długość ciągu znaków jest ważnym aspektem w programowaniu, ponieważ pozwala nam na określenie ilości znaków w danym ciągu. Jest to przydatna umiejętność, która może zostać wykorzystana w wielu różnych projektach, od prostych skryptów po bardziej zaawansowane aplikacje.

# Jak to zrobić?

## Prosty przykład
```Haskell
stringLength :: String -> Int 
stringLength "" = 0
stringLength (x:xs) = 1 + stringLength xs
```
Przykład ten po prostu przechodzi przez podany ciąg znaków i inkrementuje liczbę o 1 za każdym razem, gdy natrafi na kolejny znak. Wynik jest zwracany jako liczba całkowita reprezentująca długość ciągu.

Aby użyć tej funkcji, należy podać ciąg znaków jako argument, np.:
```Haskell
stringLength "Haskell"
```
Wynik będzie równy 7.

## Użycie wbudowanej funkcji
W Haskellu istnieje również wbudowana funkcja `length`, która wylicza długość listy lub ciągu znaków. Przykład użycia tej funkcji wygląda następująco:
```Haskell
length "Haskell"
```
Wynik będzie równy 7, podobnie jak w przykładzie z naszą własną funkcją.

# Głębsza analiza
Warto zauważyć, że funkcja `stringLength` działa także na listach, ponieważ ciągi znaków są po prostu specjalnym rodzajem listy. W takim przypadku, długość będzie liczona jako ilość elementów w liście.

Poniżej przedstawiam kilka poczytnych funkcji i operacji na listach, które wykorzystują długość jako swój element. Dzięki temu możemy jeszcze lepiej zrozumieć, jak ważna jest umiejętność wyznaczania długości listy/ciągu znaków.

- `take` - funkcja, która pobiera pierwsze n elementów z listy o długości n.
- `drop` - funkcja, która usuwa pierwsze n elementów z listy o długości n.
- `reverse` - funkcja, która odwraca kolejność elementów w liście o tej samej długości.
- `splitAt` - funkcja, która dzieli listę na dwie części: pierwsza zawiera n pierwszych elementów, a druga resztę.

# Zobacz też
- [Nauka języka Haskell](https://www.nauk-programowania.com)
- [Długość listy w Haskellu](https://www.wikibooks.org/wiki/Haskell/List_Functions#length)