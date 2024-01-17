---
title:                "Zapisywanie ciągu wielkimi literami"
html_title:           "Go: Zapisywanie ciągu wielkimi literami"
simple_title:         "Zapisywanie ciągu wielkimi literami"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Czym i dlaczego?

Kapitalizacja tekstu to proces zmiany pierwszej litery każdego słowa w zdaniu na wielką literę. Programiści wykonują tę czynność, aby poprawić czytelność tekstu i przestrzegać standardów zapisu kodu.

## Jak to zrobić:

```Go
func capitalize(s string) string {
    words := strings.Fields(s)
    for i, word := range words {
        words[i] = strings.ToUpper(word[:1]) + word[1:]
    }
    return strings.Join(words, " ")
}

// Przykładowe wywołanie funkcji capitalize:
fmt.Println(capitalize("to jest przykładowy tekst."))

/* Output:
To Jest Przykładowy Tekst.
*/
```

## Głębsza analiza:

Kapitalizacja tekstu jest powszechnie stosowana w językach programowania, które wymagają standardowego zapisu kodu. Alternatywą dla kapitalizacji jest wykorzystanie metody camelCase, gdzie pierwsza litera słowa jest małą, a kolejne są wielkimi literami. W języku Go, kapitalizacja jest realizowana przy użyciu funkcji strings.ToUpper(), która zmienia dowolny tekst na napis składający się z samych wielkich liter.

## Zobacz również:

[Go Documentation on strings](https://golang.org/pkg/strings/)