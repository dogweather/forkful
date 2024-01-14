---
title:    "PHP: Odczytywanie argumentów z wiersza poleceń"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego

Podstawowym powodem, dla którego warto poznać jak czytać argumenty wiersza poleceń w PHP, jest to, że jest to niezbędne do pisania skryptów wiersza poleceń lub aplikacji konsolowych. Wiele projektów korzysta z tego sposobu przekazywania informacji do programu, a zatem umiejętność czytania argumentów wiersza poleceń jest bardzo przydatna.

## Jak to zrobić

Aby odczytać argumenty wiersza poleceń w PHP, używamy zmiennych globalnych o nazwie ```$argc``` i ```$argv```. ```$argc``` jest liczbą argumentów, a ```$argv``` jest tablicą z argumentami przekazanymi do programu. Oto przykładowy kod:

```PHP
<?php

echo "Liczba argumentów to: " . $argc . "\n";

echo "Argumenty przekazane do programu:\n";
print_r($argv);
```

Przykładowy wynik dla wywołania programu z argumentami ```php program.php argument1 argument2``` będzie wyglądać następująco:

```PHP
Liczba argumentów to: 3
Argumenty przekazane do programu:
Array
(
    [0] => program.php
    [1] => argument1
    [2] => argument2
)
```

## Głębszy wgląd

Istnieją też inne sposoby na czytanie argumentów wiersza poleceń w PHP, takie jak wykorzystanie biblioteki ```getopt()``` lub użycie pętli ```foreach``` do iteracji po tablicy z argumentami. Ważne jest również pamiętanie o poprawnej obsłudze błędów, gdy argumenty nie są przekazane do programu lub są nieprawidłowe.

## Zobacz także

Jeśli jesteś zainteresowany/iona dalszym poznawaniem PHP, polecamy zapoznać się z poniższymi linkami:

- [Dokumentacja PHP o argumentach wiersza poleceń](https://www.php.net/manual/en/features.commandline.php)
- [Kurs PHP w Codecademy](https://www.codecademy.com/learn/learn-php)
- [Poradnik jak programować w PHP od podstaw](https://www.youtube.com/playlist?list=PLpJJtTU6FOxaSzWL9IUtq_hmE-_FJxF45)