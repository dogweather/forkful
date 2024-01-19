---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Fish Shell: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości to sposób na ustalenie, jaka będzie data za określony czas, lub jaka była data pewien czas temu. Programiści robią to, aby zarządzać wydarzeniami, które mają nastąpić lub które już się wydarzyły.

## Jak to zrobić:
Obliczanie daty w przyszłości lub przeszłości w Fish Shell można zrobić za pomocą funkcji `date`. Oto przykładowe użycie:

```Fish Shell
# Obliczanie daty za 7 dni
set future_date (date -d "+7 days" +"%Y-%m-%d")
echo $future_date

# Obliczanie daty sprzed 7 dni
set past_date (date -d "-7 days" +"%Y-%m-%d")
echo $past_date
```
Wykonanie tego kodu daje następujący output:

```Fish Shell
2022-04-28
2022-04-14
```

## Pogłębione informacje
Obliczanie daty w przyszłości lub przeszłości było popularne nawet w okresach przed komputerowych z wysokiej precyzji narzędzia takie jak zegarki słońca lub zegarki mechaniczne. Teraz jest to funkcja wbudowana w jednym z poleceń systemu Unix, `date`.

Jest wiele innych narzędzi, które mogą być użyte do obliczania daty w przeszłości lub przyszłości, w zależności od potrzeb. Na przykład, języki programowania takie jak Python, Perl i Ruby mają wbudowane biblioteki do manipulacji datą i czasem.

Implementacja tego w Fish Shell wykorzystuje faktycznie polecenie systemu Unix `date`. To polecenie pozwala na obliczanie daty w przyszłości lub przeszłości dodając lub odejmując określoną liczbę dni od bieżącej daty.

## Zobacz również
Więcej informacji na temat poleceń Fish Shell można znaleźć w dokumentacji: https://fishshell.com/docs/current/index.html

Można również zawsze skorzystać z innych źródeł, takich jak Stack Overflow lub GitHub, do rozwiązywania problemów związanych z datą i czasem w Fish Shell:
- https://stackoverflow.com/questions/32429487/date-command-in-fish-shell
- https://github.com/fish-shell/fish-shell/issues/4571