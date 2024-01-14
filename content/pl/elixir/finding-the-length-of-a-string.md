---
title:                "Elixir: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego chcielibyśmy wyznaczyć długość ciągu znaków w Elixir? Wiele zadań programistycznych wymaga manipulowania ciągami znaków, dlatego znajomość metod ich obsługi jest ważna dla każdego programisty.

## Jak To Zrobić

Najprostszym sposobem na wyznaczenie długości ciągu znaków w Elixir jest użycie funkcji `String.length()`. Przykładowy kod wyglądałby tak:

```Elixir
string = "To jest przykładowy ciąg znaków."
puts String.length(string)
```

Po uruchomieniu powyższego kodu, otrzymalibyśmy następujący wynik:

```
34
```

Możemy też obliczyć długość tylko części ciągu, wykorzystując operator wycinania `[x..y]`, gdzie `x` jest indeksem początkowym a `y` indeksem końcowym. Przykładowy kod wyglądałby tak:

```Elixir
string = "To jest przykładowy ciąg znaków."
puts String.length(string[4..10])
```

W tym przypadku, wynik będzie wynosić tylko 7, ponieważ obliczamy długość wybranego fragmentu ciągu.

## Głębszy Przegląd

Istnieje wiele innych funkcji i metod służących do manipulacji ciągami znaków w Elixir, takich jak `String.contains?`, `String.split` czy `String.downcase`. Warto zapoznać się z nimi, aby rozszerzyć swoje umiejętności programistyczne.

## Zobacz też

- Oficjalna dokumentacja Elixir: https://elixir-lang.org/docs.html
- Poradnik dla początkujących: https://howistart.org/posts/elixir/1
- Kursy i materiały: https://elixirschool.com/pl/