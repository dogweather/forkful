---
title:                "Bash: Porównywanie dwóch dat"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Bash i zajmujesz się manipulacją datami, na pewno niejednokrotnie zdarzyło Ci się konieczność porównywania dwóch dat. Dzięki temu artykułowi dowiesz się, jak to zrobić w prosty sposób oraz jakie funkcje Bash mogą Ci w tym pomóc.

## Jak to zrobić

Porównywanie dwóch dat w Bash jest dość proste i może być zrobione na kilka różnych sposobów. Jednym z najbardziej popularnych jest utworzenie dwóch zmiennych z datami oraz użycie funkcji `date` do ich formatowania. Następnie porównujemy je za pomocą operatora `>` lub `<` w warunku `if`. Przykładowy kod można zobaczyć poniżej:

```Bash
first_date="2020-05-15"
second_date="2020-05-20"

if [[ `date -d "$first_date" +"%Y%m%d"` > `date -d "$second_date" +"%Y%m%d"` ]]; then
  echo "$first_date jest późniejszą datą od $second_date"
fi
```
W powyższym przykładzie użyliśmy funkcji `date` z flagą `-d` do formatowania daty w odpowiednim formacie. Następnie za pomocą operatora `` ` `` wykonujemy te komendy i porównujemy uzyskane wartości. Jeśli pierwsza data jest większa od drugiej, wykonujemy naszą akcję (w tym przypadku wyświetlamy komunikat).

## Głębsze zanurzenie

Podczas porównywania dat warto pamiętać o kilku rzeczach. Po pierwsze, należy używać odpowiedniego formatu daty, aby porównanie było poprawne. Drugim ważnym aspektem jest uwzględnienie różnych wartości, takich jak czas czy strefa czasowa. W Bash można użyć funkcji `--reference` do porównywania daty z plikiem, co może być pomocne w niektórych przypadkach.

## Zobacz także

- Dokumentacja funkcji `date` w Bash: https://linux.die.net/man/1/date
- Przydatne skrypty Bash do pracy z datami: https://gist.github.com/joelarson4/7486674