---
title:                "PHP: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli programujesz w PHP, z pewnością spotkałeś się z sytuacją, w której potrzebujesz sprawdzić czy dany katalog istnieje. W tym wpisie dowiesz się, dlaczego jest to niezbędne oraz jak to zrobić.

## Jak to zrobić

Sprawdzenie istnienia katalogu jest bardzo proste w PHP. Wystarczy skorzystać z funkcji `is_dir()`, która zwraca wartość logiczną `true` lub `false` w zależności od tego, czy katalog istnieje czy nie. Przykładowy kod wyglądałby następująco:

```PHP
$directory = "/sciezka/do/katalogu";
 
if (is_dir($directory)) {
    echo "Katalog istnieje!";
} else {
    echo "Katalog nie istnieje.";
}
```

Przykładowy output dla istniejącego katalogu wyglądałby tak:

```
Katalog istnieje!
```

W przypadku nieistniejącego katalogu output będzie wyglądał następująco:

```
Katalog nie istnieje.
```

## Deep Dive

Funkcja `is_dir()` korzysta z systemowego wywołania `stat()` w celu sprawdzenia czy dana ścieżka jest katalogiem. Dzięki temu nie musimy sami przeszukiwać struktury systemu plików, co znacznie przyspiesza proces. Warto również zwrócić uwagę na różnicę między `is_dir()` a `file_exists()`, która również sprawdza istnienie danego pliku lub katalogu, jednak może być nieco wolniejsza w niektórych przypadkach.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej na temat operacji na katalogach w PHP, polecam zapoznać się również z poniższymi artykułami:

- [How to Create a Directory in PHP](https://www.codeofaninja.com/2017/02/php-create-directory.html)
- [How to Remove a Directory in PHP](https://www.codeofaninja.com/2017/04/php-remove-directory.html)
- [PHP Manual: is_dir() function](https://www.php.net/manual/en/function.is-dir.php)