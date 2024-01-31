---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:57:47.511534-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"

category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
"Co i Dlaczego?"

Sprawdzanie, czy katalog istnieje w PHP, to sprawdzanie obecności danego folderu na serwerze. Robimy to, żeby uniknąć błędów podczas tworzenia plików, zapisywania danych czy też ładowania zasobów z katalogów, które mogłyby nie istnieć.

## How to:
"Jak to zrobić:"

Sprawdzanie, czy katalog istnieje, jest proste. Używamy funkcji `is_dir()` do sprawdzenia, a `mkdir()` do stworzenia katalogu, jeśli nie istnieje.

```PHP
<?php
$directory = "/some/path/to/directory";

if (is_dir($directory)) {
    echo "Katalog już istnieje.";
} else {
    mkdir($directory, 0777, true);
    echo "Katalog został stworzony.";
}
?>
```
Wynik działania:
Jeśli katalog istnieje: `Katalog już istnieje.`
Jeśli katalog nie istnieje: `Katalog został stworzony.`

## Deep Dive
"Dogłębna analiza"

Sprawdzanie istnienia katalogu nie zawsze było tak proste. W starszych wersjach PHP, przed wprowadzeniem `is_dir()`, programiści musieli korzystać z innych funkcji, jak `file_exists()`, co nie zawsze dawało pewność. Alternatywnie, można użyć `file_exists()` do sprawdzania zarówno plików, jak i katalogów, ale `is_dir()` jest specyficzne dla katalogów, co czyni kod bardziej zrozumiałym.

Kiedy `mkdir()` tworzy katalog, drugi parametr określa prawa dostępu (w Unixowych systemach), a trzeci, ustawiony na `true`, pozwala na rekursywne tworzenie struktury katalogów.

Przy implementacji zawsze warto zastanowić się nad obsługą wyjątków i błędów, na przykład co zrobić, jeśli stworzenie katalogu się nie powiedzie.

## See Also
"Zobacz również"

- Oficjalna dokumentacja PHP na `is_dir()`: https://www.php.net/manual/en/function.is-dir.php
- Oficjalna dokumentacja PHP na `mkdir()`: https://www.php.net/manual/en/function.mkdir.php
- Wskazówki na temat praw dostępu do plików w PHP: https://www.php.net/manual/en/function.chmod.php
