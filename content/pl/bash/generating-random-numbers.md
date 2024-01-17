---
title:                "Generowanie losowych liczb"
html_title:           "Bash: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Czego dotyczy i dlaczego?

Generowanie losowych liczb jest popularnym narzędziem w programowaniu. Pozwala na losowe wybieranie wartości zdefiniowanych przez programistę i jest niezwykle przydatne w wielu scenariuszach, na przykład w symulacjach, grach czy testowaniu aplikacji.

## Jak to zrobić?

Możemy wygenerować losową liczbę w Bashu za pomocą funkcji $RANDOM. Przykładowo, kod ```Bash echo $RANDOM ``` zwróci losową liczbę w przedziale od 0 do 32767. Możemy także określić własny zakres, np. ```Bash echo $((RANDOM%10+1)) ``` wylosuje liczbę od 1 do 10.

## Wnikliwszy rozkład

Istnieje wiele sposobów na generowanie losowych liczb w Bashu, takich jak uzycie polecenia ```Bash shuf ``` lub funkcji ```Bash random [options] ```. Dodatkowo, możliwe jest dokładniejsze kontrolowanie rozkładu wygenerowanych liczb za pomocą algorytmów takich jak Mersenne Twister. Warto także pamiętać, że wygenerowane liczby nie są w pełni losowe, tylko pseudo-losowe, co oznacza, że stanowią ciąg liczb, który można przewidzieć.

## Zobacz także

Dla większej ilości informacji na temat generowania losowych liczb w Bashu, polecamy zapoznać się z dokumentacją Bashową oraz artykułami na temat używania funkcji $RANDOM. Możesz także poszukać innych sposobów generowania liczb, takich jak wykorzystanie zewnętrznych narzędzi lub bibliotek.