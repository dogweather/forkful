---
title:    "Go: Konkatenacja ciągów znaków"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz tworzyć programy w języku Go, z pewnością natknąłeś się na potrzebę łączenia łańcuchów znaków. W tym artykule pokażemy Ci, dlaczego ten proces jest tak ważny i jak można go łatwo wykonać.

## Jak to zrobić

Aby połączyć dwa łańcuchy znaków w języku Go, możesz użyć wbudowanej funkcji `+`. Przykładowy kod wyglądałby tak:

```Go
name := "John"
greeting := "Welcome to our blog, "
fmt.Println(greeting + name)
```
 
Wynik:

`Welcome to our blog, John`

Możesz również użyć funkcji `fmt.Sprintf()` do połączenia wielu zmiennych w jednym łańcuchu znaków. Przykładowy kod wyglądałby tak:

```Go
name := "John"
age := 30
info := fmt.Sprintf("Name: %s, Age: %d", name, age)
fmt.Println(info)
```

Wynik:

`Name: John, Age: 30`

## Głębsze zagadnienia

Podczas łączenia łańcuchów znaków w języku Go, ważne jest, aby pamiętać o wydajności. Dlatego zaleca się używanie funkcji `bytes.Buffer` lub `strings.Builder` zamiast operatora `+` lub funkcji `fmt.Sprintf()`. Powodem jest fakt, że funkcje te alokują nową pamięć za każdym razem, gdy są wywoływane, co może spowolnić działanie programu.

Innym ważnym elementem jest pamiętanie o prawidłowym formatowaniu łańcuchów znaków, szczególnie jeśli zawierają one znaki specjalne, takie jak cudzysłowy czy znaki nowej linii. W takich przypadkach należy użyć znaku ucieczki `\` przed tymi znakami, aby uniknąć błędów i nieprawidłowego formatowania.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o łączeniu łańcuchów znaków w języku Go, polecamy zapoznanie się z oficjalną dokumentacją i innymi artykułami na ten temat:

- [Dokumentacja języka Go - operacje na łańcuchach znaków](https://golang.org/pkg/strings/)
- [Blog Gopher Guides - Concatenating Strings in Go](https://gopherguides.com/articles/concatenating-strings-in-go/)
- [Medium - The Art of Concatenating Strings in Go](https://medium.com/better-programming/the-art-of-concatenating-strings-in-go-4c3a39e867c8)

Dziękujemy za przeczytanie tego artykułu. Mamy nadzieję, że teraz wiesz, dlaczego i jak łączyć łańcuchy znaków w języku Go. Życzymy Ci powodzenia w nauce tego języka i tworzeniu w nim wspaniałych aplikacji!