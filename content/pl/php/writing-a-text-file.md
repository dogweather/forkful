---
title:    "PHP: Pisanie pliku tekstowego"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego jest jedną z podstawowych czynności w programowaniu PHP. Jest to niezwykle przydatne przy tworzeniu stron internetowych, aplikacji internetowych czy skryptów. Poniżej przedstawiona jest praktyczna instrukcja, jak pisać pliki tekstowe w języku PHP.

## Jak to zrobić

Najpierw otwórzmy plik tekstowy przy pomocy funkcji `fopen()` w celu utworzenia uchwytu pliku. Następnie możemy użyć funkcji `fwrite()`, aby wpisać tekst do pliku. Na koniec zamykamy plik przy użyciu funkcji `fclose()`. Pozwala to na zapisanie wprowadzonych zmian.

```PHP
<?php

// Otwieramy plik w trybie do odczytu i zapisu
$file = fopen("tekst.txt", "w+");

// Wpisujemy nasz przykładowy tekst do pliku
fwrite($file, "Witaj, to jest przykładowy tekst dla bloga!");

// Zamykamy plik
fclose($file);

// Wyświetlamy komunikat o zapisaniu tekstu do pliku
echo "Pomyślnie zapisano tekst do pliku!";
?>
```

## Głębszy zanurzenie

W celu bardziej szczegółowego wyjaśnienia, warto wspomnieć o różnych trybach otwierania plików w PHP oraz funkcjach, które pozwalają na dokładniejszą kontrolę nad zapisywanymi danymi (np. `fputs()` czy `fpassthru()`). Warto również zwrócić uwagę na możliwość ustawienia pozycji kursora w pliku przy pomocy funkcji `fseek()`, co pozwala na dodawanie tekstu do istniejącego pliku bez nadpisywania całej zawartości.

## Zobacz również

- [Funkcja fopen() w dokumentacji PHP](https://www.php.net/manual/en/function.fopen.php)
- [Przykłady zapisu danych do plików tekstowych w PHP](https://www.php.net/manual/en/function.fwrite.php)
- [Funkcje związane z zapisywaniem danych do pliku](https://www.php.net/manual/en/ref.filesystem.php)