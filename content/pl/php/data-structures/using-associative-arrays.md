---
title:                "Korzystanie z tablic asocjacyjnych"
aliases: - /pl/php/using-associative-arrays.md
date:                  2024-01-30T19:12:55.135152-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z tablic asocjacyjnych"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Asocjacyjne tablice w PHP są jak naładowane możliwościami listy, gdzie każdy element można uzyskać przy użyciu klucza zrozumiałego dla człowieka zamiast tylko numerów. Programiści używają ich do przechowywania i manipulowania danymi bardziej intuicyjnie, co pozwala na tworzenie kodu łatwiejszego do czytania i bardziej zrozumiałego.

## Jak to zrobić:

W PHP tworzenie i używanie tablic asocjacyjnych jest proste. Oto krótki przewodnik:

```PHP
<?php
// Tworzenie tablicy asocjacyjnej
$osoba = array(
    "imie" => "Jan Kowalski",
    "wiek" => 30,
    "email" => "jan@przyklad.com"
);

// Alternatywnie, skrócona składnia tablicy
$osoba = [
    "imie" => "Jan Kowalski",
    "wiek" => 30,
    "email" => "jan@przyklad.com"
];

// Dostęp do wartości za pomocą kluczy
echo "Imię: " . $osoba["imie"] . "\n";
echo "Wiek: " . $osoba["wiek"] . "\n";
echo "Email: " . $osoba["email"] . "\n";

// Modyfikacja wartości
$osoba["wiek"] = 31;

// Dodawanie nowej pary klucz-wartość
$osoba["kraj"] = "USA";

// Iteracja przez tablicę asocjacyjną
foreach ($osoba as $klucz => $wartosc) {
    echo $klucz . ": " . $wartosc . "\n";
}

// Wynik
// Imię: Jan Kowalski
// Wiek: 31
// Email: jan@przyklad.com
// kraj: USA
?>
```

Zwróć uwagę, jak klucze mogą być dowolnym ciągiem znaków, co pozwala na dostęp do elementów używając tych kluczy zamiast indeksów numerycznych, które mogą być mniej znaczące i trudniejsze do zapamiętania.

## Szczegółowa analiza

Asocjacyjne tablice w PHP są wewnętrznie implementowane przy użyciu tablic mieszających, które zapewniają bardzo szybki dostęp do elementów za pomocą klucza, co czyni je wysoce efektywnymi do wielu zadań. Ta efektywność, w połączeniu z ich łatwością użycia, czyni tablice asocjacyjne kamieniem węgielnym programowania w PHP.

Historycznie tablice w PHP (zarówno indeksowane, jak i asocjacyjne) były niezwykle elastyczne, co pozwalało im służyć jako listy, stosy, kolejki i wiele więcej. Jednak ta elastyczność czasami może prowadzić do mniej efektywnego kodu, jeśli nie jest używana ostrożnie.

Ostatnio, z ulepszeniami w programowaniu obiektowym w PHP, niektórzy deweloperzy wolą używać obiektów do strukturalnych danych, szczególnie dla złożonych lub wzajemnie powiązanych zestawów danych. Używanie klas może oferować lepszą enkapsulację i abstrakcję, ułatwiać testowanie i klarować intencje. Jednakże, dla prostych przechowywań klucz-wartość i prostolinijnych scenariuszy manipulacji danymi, tablice asocjacyjne pozostają doskonałym wyborem ze względu na ich prostotę i intuicyjną składnię.
