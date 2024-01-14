---
title:    "PHP: Konwersja daty na ciąg znaków"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy przekonwertować datę na ciąg znaków, na przykład aby wyświetlić ją użytkownikowi lub zapisać w pliku. W tym artykule omówimy, jak wykonać tę operację przy użyciu języka PHP.

## Jak to zrobić?

```PHP
$timestamp = time(); //pobieramy aktualny czas
$date = date('Y-m-d', $timestamp); //konwertujemy na żądany format
echo $date; //wyświetlamy datę w postaci ciągu znaków
```

Kod powyżej pobiera aktualny czas za pomocą funkcji `time()`. Następnie konwertuje ten czas na wybrany format daty za pomocą funkcji `date()`. Na koniec wyświetla wynikowy ciąg znaków z datą. 

Możemy też dodać do formatu godzinę, minutę i sekundę:

```PHP
$date = date('Y-m-d H:i:s', $timestamp);
echo $date;
```

W ten sposób wyświetlimy datę w formacie `rok-miesiąc-dzień godzina:minuta:sekunda`.

## Deep Dive

Funkcja `date()` w PHP pozwala na wiele różnych możliwości formatowania daty. Jest to szczególnie przydatne, gdy chcemy wyświetlić datę w języku polskim.

Możemy dodać do naszego kodu następujący fragment:

```PHP
setlocale(LC_ALL, 'polish'); //ustawiamy język na polski
$date = strftime('%d %B %Y', $timestamp); //formatujemy datę z polskimi nazwami miesięcy i dni tygodnia
echo $date;
```

W ten sposób wyświetlimy datę w formacie `dzień miesiąc rok`, na przykład `14 stycznia 2021`.

Funkcja `setlocale()` umożliwia także formatowanie daty w innych językach. Po więcej informacji na temat możliwości funkcji `date()` i `setlocale()` zapraszamy do dokumentacji PHP.

## Zobacz też

- [Funkcja date() w dokumentacji PHP](https://www.php.net/manual/en/function.date.php)
- [Funkcja strftime() w dokumentacji PHP](https://www.php.net/manual/en/function.strftime.php)
- [Funkcja setlocale() w dokumentacji PHP](https://www.php.net/manual/en/function.setlocale.php)