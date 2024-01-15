---
title:                "Porównywanie dwóch dat"
html_title:           "Bash: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może być przydatne w wielu scenariuszach w programowaniu Bash. Na przykład, może być niezbędne do sprawdzenia, czy dana operacja została wykonana w zadanym przedziale czasu lub czy dana data jest przed lub po innej dacie. 

## Jak to zrobić

### Porównywanie dat w formacie *YYYYMMDD*

Można porównać dwie daty w formacie *YYYYMMDD* przez po prostu porównanie liczb. Na przykład, aby sprawdzić, czy dana data jest wcześniejsza lub późniejsza od innej, można użyć następującego kodu:

```Bash
if [ 20191231 -lt 20200101 ]; then
  echo "Data 20191231 jest wcześniejsza od daty 20200101"
fi
```

Output: `Data 20191231 jest wcześniejsza od daty 20200101`

### Porównywanie dat w formacie *YYYY-MM-DD*

Jeśli daty są w formacie *YYYY-MM-DD*, można wykorzystać do tego celu polecenie `date` i formatowanie wyjścia. Oto przykładowy kod:

```Bash
if [[ $(date -d '2019-12-31' +%s) -lt $(date -d '2020-01-01' +%s) ]]; then
  echo "Data 2019-12-31 jest wcześniejsza od daty 2020-01-01"
fi
```

Output: `Data 2019-12-31 jest wcześniejsza od daty 2020-01-01`

### Obsługa błędnych dat

Należy pamiętać, że w przypadku błędnych lub nieprawidłowych dat, porównanie może nie działać poprawnie. Na przykład, data 20190231 jest nieprawidłowa i może spowodować nieoczekiwane wyniki. W takim przypadku, zaleca się uprzednie sprawdzenie poprawności dat przed ich porównaniem.

## Głębsze zagadnienia

### Porównywanie z datami z przyszłością

Jeśli chcemy porównać daty w formacie *YYYY-MM-DD*, ale jedna z nich jest datą w przyszłości, można użyć flagi `-g` w poleceniu `date` lub `-o` w GNU `find`.

Przykładowy kod z wykorzystaniem polecenia `date`:

```Bash
if [[ $(date -d '2020-01-01' +%s) -lt $(date -d '+1 day' +%s) ]]; then
  echo "Data 2020-01-01 jest wcześniejsza od daty jutrzejszej"
fi
```

Output: `Data 2020-01-01 jest wcześniejsza od daty jutrzejszej`

Przykładowy kod z wykorzystaniem polecenia `find`:

```Bash
if [[ $(find -newermt '2020-01-01' -not -newermt '2020-01-02') ]]; then
  echo "Istnieje plik modyfikowany pomiędzy datami 2020-01-01 i 2020-01-02"
fi
```

Output: `Istnieje plik modyfikowany pomiędzy datami 2020-01-01 i 2020-01-02`

## Zobacz również

- [`date` man page](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [`find` man page](https://www.gnu.org/software/findutils/manual/html_node/find_html/index.html)