---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Gleam: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Usuwanie znaków pasujących do wzorca jest częstym wyzwaniem w programowaniu, szczególnie gdy piszemy skrypty przetwarzające tekst. Dzięki temu zabiegowi możemy szybko i wydajnie usunąć niechciane znaki z naszych danych, które mogą wpływać na późniejsze przetwarzanie.

## Jak to zrobić

```Gleam
import gleam/string # Importujemy bibliotekę obsługującą operacje na stringach

pattern = "[0-9]" # Tworzymy wzorzec, który odpowiada cyfrom
str = "To jest przykładowy tekst z123 liczbami 456" # Tworzymy przykładowy tekst

filtered_str = string.replace_all(pattern, "", str) # Usuwamy wszystkie znaki pasujące do wzorca
io.format("Tekst po filtrowaniu: {}", [filtered_str]) # Wypisujemy wynik: "To jest przykładowy tekst z liczbami"
```

## Pogłębiona analiza

W powyższym przykładzie użyliśmy funkcji `replace_all` z biblioteki `gleam/string`, która za pomocą wzorca usuwa wszystkie znaki pasujące do niego z danego tekstu. Wzorzec możemy tworzyć za pomocą wyrażeń regularnych, co daje nam dużą elastyczność w usuwaniu różnorodnych znaków. Funkcja `replace_all` zwraca nowy tekst, więc nie modyfikujemy oryginalnego. 

Możemy także wykorzystać pętlę `for` i warunek `if` do bardziej złożonych operacji usunięcia znaków pasujących do wzorca. Przykładowo:

```Gleam
import gleam/string

pattern = "[!@#$%^&*]" # Tworzymy wzorzec, który odpowiada wykrzyknikom, znakom "@" przez "%" oraz "&" i "*"
str = "!!!To! jest! przykładowy% tekst! z& niepotrzebnymi $znakami@#$%^"

filtered_str = for char in string.to_list(str) do # Zamieniamy string na listę znaków i iterujemy przez nią
  if string.matches(pattern, string.from_char_list([char])) do # Sprawdzamy czy dany znak pasuje do wzorca
    nil # Jeśli tak to pomijamy go
  else
    char # W przeciwnym razie dodajemy go do nowej listy
  end
end
|> string.from_char_list() # Zamieniamy z powrotem na string

io.format("Tekst po filtrowaniu: {}", [filtered_str]) # Wypisujemy wynik: "To jest przykładowy tekst z niepotrzebnymi znakami"
```

## Zobacz również

- [Dokumentacja biblioteki gleam/string](https://gleam.run/documentation/stdlib/string/)
- [Tutorial z wyrażeniami regularnymi](https://www.w3schools.com/python/python_regex.asp)
- [Inne przydatne funkcje do operacji na stringach w Gleam](https://gleam.run/documentation/stdlib/#string)