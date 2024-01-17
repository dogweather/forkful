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

## Co to jest i dlaczego to robimy?

Odczytywanie argumentów z wiersza poleceń to proces, w którym programista pobiera dane dostarczone przez użytkownika w wierszu poleceń systemu operacyjnego. Jest to szczególnie ważne w programowaniu PHP, ponieważ pozwala nam na interakcję z użytkownikiem w czasie wykonywania programu, dzięki czemu program może być w pełni funkcjonalny i dostosowywalny.

## Jak to zrobić:

### Przykład 1: Proste odczytywanie argumentów z wiersza poleceń
```PHP
<?php
// Ustawienie dostępnych opcji argumentów
$options = getopt("f:t:m:");

// Wyświetlenie argumentów i wartości
echo "Początek: ".$options["f"]."\n";
echo "Koniec: ".$options["t"]."\n";
echo "Wiadomość: ".$options["m"]."\n";
```

### Przykład 2: Wykorzystanie domyślnych wartości dla niepodanych argumentów
```PHP
<?php
// Ustawienie domyślnych wartości dla argumentów
$defaults = array(
    "f" => "2019-01-01",
    "t" => "2019-12-31",
    "m" => "Wesołego Nowego Roku!"
);

// Zastosowanie domyślnych wartości dla niepodanych argumentów
$options = getopt("f:t:m:", $defaults);

// Wyświetlenie argumentów i wartości
echo "Początek: ".$options["f"]."\n";
echo "Koniec: ".$options["t"]."\n";
echo "Wiadomość: ".$options["m"]."\n";
```

### Przykład 3: Odczytywanie argumentów jako tablica
```PHP
<?php
// Ustawienie opcji "m" na częstotliwość
$options = getopt("f:t:m:");

// Zamiana argumentu "m" na tablicę
$freq = explode(",", $options["m"]);

// Wyświetlenie poszczególnych częstotliwości
foreach($freq as $f){
    echo "Częstotliwość: ".$f."\n";
}
```

## Deep Dive:

Historia:
Odczytywanie argumentów z wiersza poleceń jest ważnym elementem programowania od początku istnienia systemów operacyjnych. Wcześniej musieliśmy pisać skrypty wiersza poleceń i wykorzystywać zmienne środowiskowe, aby dostarczyć dane do programów. Dzięki odczytywaniu argumentów w PHP, możemy wykonać te same zadania w wygodny sposób bez korzystania z dodatkowych skryptów.

Alternatywy:
Choć odczytywanie argumentów jest niezbędnym elementem w programowaniu PHP, istnieją również alternatywy, takie jak wykorzystanie formularzy lub interaktywnych menu, aby uzyskać dane od użytkownika. Jednak odczytywanie argumentów jest szybkim i prostym sposobem na interakcję z użytkownikiem w czasie wykonywania programu.

Szczegóły implementacji:
W PHP istnieje wbudowana funkcja `getopt()`, która jest wykorzystywana do odczytywania argumentów z wiersza poleceń. Funkcja ta przyjmuje dwa parametry: pierwszy to ciąg znaków określający dostępne opcje argumentów, a drugi to opcjonalna tablica z domyślnymi wartościami dla argumentów. Funkcja ta zwraca tablicę z wartościami przypisanymi do argumentów. Dzięki wykorzystaniu tej funkcji, odczytywanie argumentów jest szybkie i łatwe w implementacji.

## Zobacz też:

1. Dokumentacja PHP: https://www.php.net/manual/en/function.getopt.php
2. Przykładowe użycie argumentów w wierszu poleceń: https://www.anil2u.info/2011/02/getopt-php-function-to-read-command-line-and-display-output/
3. Blog o wykorzystaniu argumentów w wierszu poleceń: https://girishjoshi.io/blog/php-getopt-command-line-options-in-php/