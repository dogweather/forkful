---
title:                "PHP: Sprawdzanie czy istnieje katalog"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą PHP i regularnie pracujesz z plikami, na pewno zdarzyło ci się kiedyś potrzebować sprawdzenia, czy dany katalog istnieje. Dlatego dzisiaj przyjrzymy się, jak to zrobić w php.

## Jak to zrobić

Sprawdzenie, czy katalog istnieje, można wykonać za pomocą funkcji `is_dir()`. Przyjmuje ona jako argument nazwę katalogu i zwraca `true`, jeśli istnieje, lub `false`, jeśli nie istnieje. Oto prosty przykład kodu:

```PHP
if (is_dir("nazwa_katalogu")) {
    echo "Katalog istnieje.";
}else{
    echo "Katalog nie istnieje.";
}
```

## Wgląd w głębię

Funkcja `is_dir()` wykorzystuje strumienie (streams) PHP, aby przetestować, czy katalog istnieje. Przed wykonaniem funkcji, najpierw odbywa się próba otwarcia katalogu w strumieniu, a następnie sprawdzenie, czy nie wystąpiły błędy. Jeśli funkcja zwraca `true`, oznacza to, że strumień został pomyślnie otwarty, co oznacza, że katalog istnieje.

Ponadto, funkcja `is_dir()` nie tylko sprawdza, czy podany katalog istnieje, ale także, czy jest to katalog. Jeśli podana nazwa odnosi się do pliku, a nie katalogu, funkcja zwróci `false`.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o pracy z plikami w PHP, zapoznaj się z poniższymi artykułami:

- [Otwieranie plików w PHP](https://pl.wikibooks.org/wiki/PHP/Czytanie_i_pisanie_plik%C3%B3w)
- [Tworzenie i usuwanie katalogów w PHP](https://www.php.net/manual/en/function.mkdir.php)
- [Podstawy manipulacji plikami w PHP](https://www.php.net/manual/en/function.file.php#refsect1-function.file-examples)