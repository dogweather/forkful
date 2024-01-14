---
title:                "Bash: Konwertowanie daty na ciąg znaków"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędna w wielu programach i skryptach Bash. Może to być przydatne, gdy chcesz zapisać datę jako część nazwy pliku lub wyświetlić ją w czytelny sposób dla użytkownika. W tym artykule dowiesz się, jak w prosty sposób przeprowadzić tę konwersję.

## Jak to zrobić

Getting the current date:
```Bash
current_date=$(date +"%Y-%m-%d")
echo $current_date
```
Output:
```
2021-06-26
```
Konwertowanie daty na ciąg znaków:
```Bash
date_as_string=$(date +"%A, %B %d, %Y")
echo $date_as_string
```
Output:
```
Saturday, June 26, 2021
```

## Deep Dive

Konwersja daty na ciąg znaków wymaga użycia funkcji `date` wraz z odpowiednią opcją formatowania, wyrażoną przez  znak procentu `%`. W przykładach wykorzystujemy opcję `%Y`, aby zwrócić rok, `%m` dla miesiąca, a `%d` dla dnia. Dzięki temu możemy otrzymać datę w różnych formatach, jakie tylko sobie wymarzymy.

Ponadto, funkcja `date` może również przyjmować argumenty, takie jak `-d` dla daty, której chcemy dokonać konwersji. W ten sposób możemy uzyskać ciągi znaków dla różnych dat niż bieżąca.

## Zobacz także

- [Przydatne opcje funkcji `date` w Bash](https://www.computerhope.com/unix/bash/date.htm)
- [Dokumentacja funkcji `date` w Bash](https://www.man7.org/linux/man-pages/man1/date.1.html)
- [Formatowanie daty w Bash](https://bash.cyberciti.biz/guide/Date_formatting)