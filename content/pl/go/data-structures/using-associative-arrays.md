---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:57.459625-07:00
description: "Tablice asocjacyjne, znane w Go jako mapy, pozwalaj\u0105 przechowywa\u0107\
  \ pary klucz-warto\u015B\u0107, gdzie ka\u017Cdy unikalny klucz odpowiada warto\u015B\
  ci. Programi\u015Bci u\u017Cywaj\u0105\u2026"
lastmod: '2024-03-11T00:14:08.012410-06:00'
model: gpt-4-0125-preview
summary: "Tablice asocjacyjne, znane w Go jako mapy, pozwalaj\u0105 przechowywa\u0107\
  \ pary klucz-warto\u015B\u0107, gdzie ka\u017Cdy unikalny klucz odpowiada warto\u015B\
  ci. Programi\u015Bci u\u017Cywaj\u0105\u2026"
title: "U\u017Cywanie tablic asocjacyjnych"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tablice asocjacyjne, znane w Go jako mapy, pozwalają przechowywać pary klucz-wartość, gdzie każdy unikalny klucz odpowiada wartości. Programiści używają map do efektywnego odnajdywania danych, modyfikacji oraz do utrzymania kolekcji elementów, które można szybko uzyskać używając unikalnych kluczy.

## Jak to zrobić:

Tworzenie i inicjalizowanie mapy w Go można zrealizować na różne sposoby. Oto podstawowy przykład, aby zacząć:

```go
package main

import "fmt"

func main() {
    // Deklarowanie i inicjalizacja mapy
    kolory := map[string]string{
        "czerwony": "#FF0000",
        "zielony":  "#00FF00",
        "niebieski": "#0000FF",
    }

    fmt.Println(kolory)
    // Wyjście: map[niebieski:#0000FF zielony:#00FF00 czerwony:#FF0000]
}
```

Aby dodać lub zaktualizować elementy, przypisujesz wartość do klucza w następujący sposób:

```go
kolory["biały"] = "#FFFFFF"
fmt.Println(kolory)
// Wyjście: map[niebieski:#0000FF zielony:#00FF00 czerwony:#FF0000 biały:#FFFFFF]
```

Dostęp do wartości przez jej klucz jest prosty:

```go
fmt.Println("Kod heksadecymalny dla czerwonego to:", kolory["czerwony"])
// Wyjście: Kod heksadecymalny dla czerwonego to: #FF0000
```

Aby usunąć element, użyj funkcji `delete`:

```go
delete(kolory, "czerwony")
fmt.Println(kolory)
// Wyjście: map[niebieski:#0000FF zielony:#00FF00 biały:#FFFFFF]
```

Iteracja po mapie odbywa się przy użyciu pętli for:

```go
for kolor, heks := range kolory {
    fmt.Printf("Klucz: %s Wartość: %s\n", kolor, heks)
}
```

Pamiętaj, że mapy w Go są nieuporządkowane. Kolejność iteracji nie jest gwarantowana.

## W głębi

W Go, mapy są implementowane jako tablice haszujące. Każdy wpis w mapie składa się z dwóch elementów: klucza i wartości. Klucz jest haszowany, aby przechować wpis, co pozwala na operacje w stałym czasie dla małego zestawu danych i średnią złożoność czasową O(1) przy odpowiednim haszowaniu, która może się pogorszyć do O(n) w najgorszym przypadku przy wielu kolizjach hasz.

Istotna uwaga dla nowych programistów Go to, że typy map są typami referencyjnymi. Oznacza to, że kiedy przekazesz mapę do funkcji, wszelkie zmiany dokonane na mapie w tej funkcji są widoczne dla wywołującego. Jest to inne niż np. przekazanie struktury do funkcji, gdzie struktura jest kopiowana, chyba że jest przekazywana przez wskaźnik.

Chociaż mapy są niezwykle wszechstronne i efektywne dla większości przypadków użycia związanych z tablicami asocjacyjnymi, w aplikacjach krytycznych pod względem wydajności może być korzystne użycie struktur danych o bardziej przewidywalnych charakterystykach wydajności, zwłaszcza jeśli dystrybucja kluczy może powodować częste kolizje.

Inną alternatywą do rozważenia jest `sync.Map`, dostępny od Go 1.9, zaprojektowany do przypadków użycia, gdzie klucze są zapisywane tylko raz, ale odczytywane wiele razy, oferujący poprawę wydajności w tych scenariuszach. Jednak dla konwencjonalnych aplikacji Go, regularne użycie map jest idiomatyczne i często zalecane ze względu na jego prostotę i bezpośrednie wsparcie w języku.
