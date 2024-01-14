---
title:                "PHP: Odczyt pliku tekstowego"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, wiele aplikacji internetowych wymaga przetworzenia plików tekstowych, takich jak pliki konfiguracyjne czy pliki dziennika. Dlatego też, umiejętność czytania plików tekstowych jest niezwykle przydatna dla każdego programisty PHP. W tym artykule dowiesz się, jak w prosty sposób odczytać plik tekstowy za pomocą PHP.

## Jak To Zrobić

Zacznijmy od stworzenia prostego pliku tekstowego o nazwie "tekst.txt". W pliku tym wpiszmy kilka linii tekstu, które będą służyć jako dane przykładowe. Następnie, w swoim skrypcie PHP, możemy wykorzystać funkcję "file_get_contents()" aby odczytać ten plik.

```PHP
<?php
$plik = file_get_contents("tekst.txt");
echo $plik;
```

W powyższym przykładzie, funkcja "file_get_contents()" odczytuje zawartość pliku tekst.txt i przypisuje ją do zmiennej $plik. Następnie, za pomocą polecenia "echo", wyświetlamy zawartość tej zmiennej, co w efekcie wyświetla całą zawartość pliku tekst.txt.

Aby odczytać plik linia po linii, możemy wykorzystać funkcję "file()", która zapisuje każdą linię pliku jako element tablicy.

```PHP
<?php
$plik = file("tekst.txt");
foreach($plik as $linia) {
    echo $linia;
}
```

W powyższym przykładzie, funkcja "file()" odczytuje plik tekst.txt i zapisuje go jako tablicę o nazwie $plik. Następnie, za pomocą pętli "foreach", iterujemy przez każdy element tablicy i wyświetlamy go za pomocą polecenia "echo".

## Głębsze Zanurzenie

PHP oferuje także wiele innych funkcji, które umożliwiają zaawansowane operacje na plikach tekstowych, takich jak "fopen()" do otwierania pliku, "feof()" do sprawdzania czy osiągnięto koniec pliku, czy "fgets()" do odczytywania pojedynczej linii pliku.

Korzystanie z tych funkcji może być przydatne, gdy chcemy przetwarzać dane w bardziej złożony sposób lub manipulować zawartością pliku przed wyświetleniem go lub zapisaniem do innego pliku.

## Zobacz też

- [Dokumentacja PHP: file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [Dokumentacja PHP: file()](https://www.php.net/manual/en/function.file.php)
- [Dokumentacja PHP: fopen()](https://www.php.net/manual/en/function.fopen.php)