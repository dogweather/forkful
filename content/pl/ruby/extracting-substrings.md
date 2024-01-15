---
title:                "Wycinanie podciągów"
html_title:           "Ruby: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z napisami lub dokumentami tekstowymi, w pewnym momencie może pojawić się potrzeba wyciągnięcia pewnego fragmentu tekstu ze stringa. W Ruby istnieje wiele sposobów na wyciąganie podciągów, co pozwala na wygodne i skuteczne manipulowanie tekstem.

## Jak to zrobić

W Ruby do wyciągania podciągów służy metoda `slice`, która przyjmuje dwa argumenty - początkowy indeks oraz długość. Przykłady wykorzystania tej metody prezentują się następująco:

```Ruby
"Hello World".slice(0, 5) # output: "Hello"
"Hello World".slice(6, 5) # output: "World"
```

Możemy również wyciągnąć podciąg za pomocą operatorów rzutowania:

```Ruby
"Hello World"[0, 5] # output: "Hello"
"Hello World"[6, 5] # output: "World"
```

Jeśli chcemy wyciągnąć podciąg od danego indeksu do końca stringa, możemy użyć tylko jednego argumentu bez określania długości:

```Ruby
"Hello World"[6..] # output: "World"
```

Możemy również wyciągnąć podciąg od końca stringa, używając ujemnych indeksów:

```Ruby
"Hello World"[-5..-1] # output: "World"
```

## Deep Dive

W Ruby mamy dostęp do wielu innych metod służących do wyciągania podciągów, takich jak `slice!`, `substring` czy `scan`. Każda z nich ma nieco inne zachowanie i może być bardziej odpowiednia w zależności od konkretnego przypadku.

Warto również zapoznać się z regularnymi wyrażeniami, które są bardzo potężnym narzędziem do manipulowania tekstem. Wiele problemów związanych z wyciąganiem podciągów można rozwiązać za pomocą wyrażeń regularnych.

## Zobacz także

* [Dokumentacja Ruby o metodzie `slice`](https://ruby-doc.org/core-3.0.1/String.html#method-i-slice)
* [Artykuł na temat wyrażeń regularnych w Ruby](https://medium.com/better-programming/why-learning-regular-expressions-is-hard-and-how-to-deal-with-them-fb9f57f24b5c)