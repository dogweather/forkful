---
title:    "PHP: Sprawdzanie czy istnieje katalog."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą PHP, z pewnością zdarzyło Ci się kiedyś potrzebować sprawdzić, czy dany katalog istnieje na Twoim serwerze. Może to być konieczne przy tworzeniu plików lub przeprowadzaniu operacji na plikach w danym katalogu. Dlatego w tym artykule przekonasz się, dlaczego warto umieć sprawdzić istnienie katalogu oraz jak to zrobić w praktyce.

## Jak to zrobić

Sprawdzenie istnienia katalogu w języku PHP jest bardzo proste. Najważniejszą funkcją w tym przypadku jest `file_exists()`, która zwraca wartość logiczną `true` lub `false` w zależności od istnienia podanego katalogu. Przykładowy kod można zapisać w następujący sposób:

```PHP
if (file_exists('nazwa_katalogu')) {
    echo "Katalog istnieje";
} else {
    echo "Katalog nie istnieje";
}
```

Powyższy kod będzie działał dla bezpośredniej ścieżki do katalogu. Jeśli jednak chcesz sprawdzić istnienie katalogu w podkatalogu, musisz podać pełną ścieżkę do tego katalogu.

```PHP
if (file_exists('sciezka/do/katalogu/nazwa_katalogu')) {
    echo "Katalog istnieje";
} else {
    echo "Katalog nie istnieje";
}
```

Możesz również użyć funkcji `is_dir()`, która zwraca `true` tylko wtedy, gdy podana ścieżka wskazuje na istniejący katalog. Przykładowy kod będzie wyglądał następująco:

```PHP
if (is_dir('nazwa_katalogu')) {
    echo "Katalog istnieje";
} else {
    echo "Katalog nie istnieje";
}
```

Dzięki temu możesz mieć pewność, że podana ścieżka prowadzi do istniejącego katalogu.

## Wnikliwa analiza

Oprócz wymienionych już funkcji, istnieje również możliwość użycia funkcji `scandir()`, która zwraca tablicę zawierającą nazwy plików i katalogów w danym katalogu. Dzięki niej możesz łatwo sprawdzić, czy dany katalog jest pusty lub czy zawiera jakieś pliki.

```PHP
$files = scandir('nazwa_katalogu');

if (empty($files)) {
    echo "Katalog jest pusty";
} else {
    echo "Katalog zawiera pliki";
}
```

Inną przydatną funkcją jest `glob()`, która pozwala na wyszukiwanie plików lub katalogów z danym wzorcem nazwy. W ten sposób możesz łatwo sprawdzić, czy w danym katalogu znajdują się pliki spełniające określone kryteria.

```PHP
$files = glob('nazwa_katalogu/*.txt');

if (empty($files)) {
    echo "Katalog nie zawiera plików spełniających kryteria";
} else {
    echo "Katalog zawiera pliki spełniające kryteria";
}
```

Sprawdzanie istnienia katalogu jest także przydatne przy pracy z plikami. Możesz np. tworzyć pliki lub przeprowadzać operacje na nich tylko wtedy, gdy dany katalog już istnieje.

## Zobacz także

- Dokumentacja PHP: [file_exists()](https://www.php.net/manual/en/function.file-exists.php)
- Dokumentacja PHP: [is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- Dokumentacja PHP: [scandir()](https://www.php.net/manual/en/function.scandir.php)
- Dokumentacja PHP: [glob()](https://www.php.net/manual/en/function.glob.php)
- Przykładowy kod na GitHubie: [Checking if a directory exists in PHP](https://github.com/example/checking-directory-exists-php)