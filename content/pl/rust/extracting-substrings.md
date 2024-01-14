---
title:    "Rust: Wycinanie podciągów"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Extrahowanie podciągów, czyli wyodrębnianie części tekstu z ciągłego łańcucha znaków, jest niezbędnym elementem wielu projektów programistycznych. Jest to szczególnie przydatne, gdy potrzebujemy uzyskać dostęp do określonych fragmentów tekstu lub podzielić go na mniejsze części do dalszego przetwarzania. W języku programowania Rust istnieją wiele sposobów na wydajne i bezpieczne wyodrębnianie podciągów, dlatego postanowiliśmy przygotować ten poradnik, aby pomóc w zrozumieniu tego zagadnienia.

## Jak to zrobić

Aby wyodrębnić podciąg w języku Rust, musimy użyć metody `get()` lub `slice()` na obiekcie typu `String`. Poniżej przedstawiamy prosty przykład kodu, który wyodrębnia podciąg "Rust" z tekstu "Programowanie w Rust jest super!".

```Rust
let tekst = String::from("Programowanie w Rust jest super!");

//Używamy metody get() do wyodrębnienia podciągu od indeksu 17 do 21
let podciag = tekst.get(17..21);

//Alternatywnie, możemy też użyć metody slice() i podać indeksy w nawiasach kwadratowych
let podciag = &tekst[17..21];

//Wyświetlenie wyniku
println!("Wyodrębniony podciąg: {}", podciag);
```

Output:

```
Wyodrębniony podciąg: Rust
```

Na powyższym przykładzie widać, że metoda `get()` lub `slice()` przyjmuje argument w postaci zakresu indeksów, które określają początek i koniec wyodrębnianego podciągu. Dodatkowo, możemy użyć metody `len()` na obiekcie typu `String`, aby uzyskać długość tekstu i łatwiej określić odpowiednie indeksy.

Ponadto, istnieje również możliwość wykorzystania metody `split()` w celu podzielenia tekstu na podciągi zgodnie z określonym separatorem. Przykład takiego użycia został przedstawiony poniżej.

```Rust
let tekst = String::from("Jestem|dzielony|na|podciagi");

//Używamy metody split() i podajemy separator "|"
let podciagi: Vec<&str> = tekst.split("|").collect();

//Wyświetlenie wyników
println!("Wyodrębnione podciągi: {:?}", podciagi);
```

Output:

```
Wyodrębnione podciągi: ["Jestem", "dzielony", "na", "podciagi"]
```

## Deep Dive

W języku Rust, pulia indeksowana jest od 0, czyli pierwszy znak tekstowy znajduje się pod indeksem 0, drugi pod indeksem 1, itd. Warto zauważyć, że indeks działania na tekstach kończy się na poprzednim indeksie bez znaczenia czy ostatni indeks został podany w argumentach metody `get()` lub `slice()`.

Ponadto, w przypadku użycia metody `slice()`, jest możliwość podania tylko jednego argumentu - wtedy tekst zostanie wyodrębniony od podanego indeksu do końca.

## Zobacz też

- [Dokumentacja Rust na temat wyodrębniania podciągów](https://doc.rust-lang.org/std/primitive.str.html#method.get)
- [Poradnik: Jak używać metod get() i slice() w Rust](https://www.educative.io/blog/learn-rust-get-slice)
- [Wideo-tutorial: Wyodrębnianie podciągów z tekstu w Rust