---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "PHP: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest nieodłączną częścią wielu dziedzin, a znajomość języków programowania jest niezbędna do wykonywania wielu zadań. Język PHP jest jednym z najpopularniejszych języków programowania na świecie, więc jest ważne dla programistów posiadanie wiedzy o jego różnych funkcjonalnościach. Jedną z takich funkcjonalności są argumenty wiersza poleceń, które pozwalają na przekazywanie parametrów podczas uruchamiania skryptu PHP z konsoli. W tym artykule dowiesz się, dlaczego warto poznać tę funkcjonalność i jak ją wykorzystać.

## Jak to zrobić

W celu odczytania argumentów wiersza poleceń w PHP, musimy skorzystać z tablicy globalnej o nazwie `$argv`. Jest to tablica, która zawiera wszystkie przekazane parametry wiersza poleceń jako swoje elementy. W razie potrzeby, możemy również skorzystać z tablicy globalnej `$argc`, która przechowuje liczbę przekazanych argumentów.

Przykładowy kod w języku PHP wykorzystujący argumenty wiersza poleceń może wyglądać tak:

```php
<?php
// Sprawdzamy czy podane zostały odpowiednie argumenty
if ($argc < 3) {
    echo "Użycie: php skrypt.php argument1 argument2";
    exit;
}

// Przypisujemy przekazane argumenty do zmiennych
$arg1 = $argv[1];
$arg2 = $argv[2];

// Wyświetlamy argumenty wraz z tekstem pomocniczym
echo "Pierwszy argument: " . $arg1 . "\n";
echo "Drugi argument: " . $arg2;
```

W powyższym przykładzie wykorzystujemy argumenty w celu przypisania wartości do zmiennych oraz wyświetlamy je na konsoli wraz z pomocniczym tekstem. Przykładowe wywołanie skryptu z konsoli wyglądałoby tak:

```
php skrypt.php Hello World
```

Wynikiem działania skryptu byłoby:

```
Pierwszy argument: Hello
Drugi argument: World
```

Możemy również wykorzystać argumenty wiersza poleceń do przekazania wartości do naszej aplikacji lub skryptu, na przykład podczas wywołania przez zewnętrzny program lub skrypt powłoki. Dzięki temu nasz skrypt będzie bardziej uniwersalny i łatwiejszy w użyciu dla innych użytkowników.

## Głębokie zanurzenie

W przypadku, gdy nasz skrypt ma przyjmować wiele argumentów lub parametry o różnych typach danych, musimy zadbać o odpowiednie walidowanie i konwersję wartości. W celu ułatwienia tego procesu, można skorzystać z funkcji `getopt()` lub wykorzystać zewnętrzną bibliotekę taką jak np. "symfony/console".

Istnieje również możliwość odczytania argumentów wiersza poleceń podczas działania skryptu PHP, dzięki wykorzystaniu funkcji `getopt()` w połączeniu z pętlą `while` i funkcją `fgets()`, która pozwala na pobieranie pojedynczych linii z konsoli.

## Zobacz również

- [Dokumentacja PHP - Argumenty wiersza poleceń](https://www.php.net/manual/en/reserved.variables.argv.php)
- [Biblioteka "symfony/console" do obsługi argumentów wiersza poleceń](https://github.com/symfony/console)