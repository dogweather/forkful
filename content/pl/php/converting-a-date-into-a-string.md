---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "PHP: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem w programowaniu musimy przekonwertować datę na ciąg znaków, na przykład w celu wyświetlenia jej w przyjaznym dla użytkownika formacie. W tym artykule dowiesz się, jak w łatwy sposób wykonać taką konwersję w PHP.

## Jak to zrobić

Zacznijmy od wykorzystania wbudowanej funkcji `date()` w PHP, która pozwala na formatowanie daty według podanych parametrów. Poniższy kod zamienia aktualną datę na ciąg znaków w formacie `d-m-Y`, czyli dzień-miesiąc-rok.

```PHP
$date = date("d-m-Y");
echo $date;
```

Output: `18-07-2021`

Możemy także wykorzystać funkcję `strtotime()`, która pozwala na konwersję daty podanej w formacie tekstowym na datę w formacie timestamp. Następnie, korzystając z funkcji `date()`, możemy zmienić format tego timestampu na dowolny, który potrzebujemy. Na przykład:

```PHP
$date = "July 18, 2021";
$timestamp = strtotime($date);
$converted_date = date("d/m/Y", $timestamp);
echo $converted_date;
```

Output: `18/07/2021`

Możemy też wykorzystać wbudowany dodatek `DateTime`, który umożliwia wygodną manipulację datami. Poniższy kod zamienia obecną datę na ciąg znaków w formacie `Y-m-d`, czyli rok-miesiąc-dzień.

```PHP
$date = new DateTime();
$date_string = $date->format('Y-m-d');
echo $date_string;
```

Output: `2021-07-18`

## Głębsze rozważania

W języku PHP istnieje wiele funkcji i narzędzi pozwalających na konwersję daty na string. Co ciekawe, niektóre z nich, takie jak `strftime()`, umożliwiają także wyświetlenie daty w różnych językach, uwzględniając ustawienia lokalne. Ponadto, warto pamiętać, że PHP posiada wiele wbudowanych formatów dat, które można wykorzystać w funkcji `date()`, zmieniając jedynie podany parametr. Dzięki temu możliwa jest szybka i prosta konwersja daty na dowolny format.

## Zobacz także

- Dokumentacja PHP na temat funkcji `date()`: https://www.php.net/manual/en/function.date.php
- Dokumentacja PHP na temat narzędzia `DateTime`: https://www.php.net/manual/en/class.datetime.php
- Opis funkcji `strftime()`: https://www.php.net/manual/en/function.strftime.php