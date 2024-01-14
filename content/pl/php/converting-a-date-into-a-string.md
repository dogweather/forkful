---
title:                "PHP: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest jednym z podstawowych zadań, z którymi spotyka się każdy programista PHP. Jest to niezbędne w celu wyświetlenia daty w czytelny sposób dla użytkowników czy też przechowywania jej w bazie danych. W tym krótkim artykule zobaczysz, jak w prosty sposób przekształcić datę w PHP w ciąg znaków.

## Jak to zrobić?

Najprostszym sposobem na konwersję daty na ciąg znaków jest użycie funkcji `date()` wraz z odpowiednim formatem. Na przykład, jeśli chcemy wyświetlić datę w formacie "Dzień Tygodnia, Dzień Miesiąca Rok", możemy użyć następującego kodu:

```PHP
<?php
$date = date('l, j F Y');
echo $date; // Output: Środa, 15 Kwietnia 2020
?>
```

Jak widać, formatowanie daty jest bardzo intuicyjne i pozwala na wyświetlenie jej w różnych formatach. Przykładowe formaty, które możesz wykorzystać, to: `l` - dzień tygodnia w formacie tekstowym, `j` - dzień miesiąca bez zera wiodącego, `F` - nazwa miesiąca w formacie tekstowym, `Y` - rok w formacie czterocyfrowym, itd. Pełną listę możliwych formatów można znaleźć w dokumentacji PHP.

## Głębszy wgląd

Konwersja daty na ciąg znaków może być nieco bardziej skomplikowana, jeśli chcemy ją dostosować do swoich potrzeb. Dlatego, jeśli potrzebujesz bardziej rozbudowanej konwersji, warto zapoznać się z funkcją `strftime()`, która pozwala na wykorzystanie zaawansowanych formatów i opcji. Możesz również użyć bibliotek zewnętrznych, takich jak Carbon, która udostępnia wiele przydatnych funkcji do pracy z datami w PHP.

## Zobacz również

- [Dokumentacja PHP na temat funkcji `date()`](https://www.php.net/manual/en/function.date.php)
- [Dokumentacja PHP na temat funkcji `strftime()`](https://www.php.net/manual/en/function.strftime.php)
- [Biblioteka Carbon do zarządzania datami w PHP](https://carbon.nesbot.com/)