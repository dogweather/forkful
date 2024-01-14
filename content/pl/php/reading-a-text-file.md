---
title:    "PHP: Odczytywanie pliku tekstowego"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego?

Czy kiedykolwiek znalazłeś się w sytuacji, w której musiałeś przeczytać zawartość pliku tekstowego w swoim kodzie PHP? Może chcesz odczytać dane z pliku dziennika lub wyświetlić zawartość pliku konfiguracyjnego dla Twojej aplikacji. Niezależnie od powodu, czytanie pliku tekstowego jest niezbędnym narzędziem w programowaniu PHP i w tym artykule pokażemy Ci, jak to zrobić.

## Jak to zrobić?

Aby odczytywać pliki tekstowe w kodzie PHP, używamy funkcji `file_get_contents()`. Poniżej znajdziesz przykłady użycia tej funkcji wraz z odpowiadającym im wynikiem.

```PHP
// Przykład 1 - Odczyt całego pliku tekstowego
$content = file_get_contents('example.txt');
echo $content; // wyświetli zawartość pliku

// Przykład 2 - Odczyt wiersza po wierszu
$lines = file('example.txt');
foreach ($lines as $line) {
    echo $line . '<br>'; // wyświetli każdy wiersz z pliku 
}

// Przykład 3 - Odczyt pliku jako tablica z wykorzystaniem opcji SKIP_EMPTY_LINES
$content = file('example.txt', FILE_SKIP_EMPTY_LINES);
foreach ($content as $line) {
    echo $line . '<br>'; // wyświetli każdy wiersz pliku, ale pominie puste linie
}
```

## Głębsze zagadnienia

Podczas odczytywania plików tekstowych zauważysz, że bardziej wydajne jest używanie funkcji `file()` zamiast `file_get_contents()`. Funkcja ta zwraca zawartość pliku w postaci tablicy, co może być wygodne przy przeprowadzaniu dodatkowych operacji na pliku. Możesz również przekazać opcje jako drugi parametr funkcji `file()`, co pozwala na jeszcze większą kontrolę nad odczytywanym plikiem.

## Zobacz także
- [Dokumentacja PHP - file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [Dokumentacja PHP - file()](https://www.php.net/manual/en/function.file.php)
- [Blog KursPHP.pl - Odczytywanie plików tekstowych w PHP](https://kursphp.pl/odczytywanie-plikow-tekstowych-w-php/)