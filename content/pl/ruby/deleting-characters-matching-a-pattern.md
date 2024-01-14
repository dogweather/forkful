---
title:                "Ruby: Usuwanie znaków pasujących do wzorca"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Dlaczego usuwać znaki pasujące do wzorca?

Usuwanie określonych znaków z tekstu często jest ważną czynnością w programowaniu. Może to pomóc w filtrowaniu i przetwarzaniu danych, usunięciu białych znaków lub poprawieniu nieprawidłowych znaków. Podczas gdy w Ruby istnieje wiele metod do usuwania znaków, jedną z najbardziej użytecznych jest usuwanie znaków pasujących do wzorca.

## Jak to zrobić?

Wykorzystując metodę `gsub` możemy usunąć wszystkie znaki pasujące do danego wzorca z określonego tekstu. Na przykład, aby usunąć wszystkie znaki cyfr z ciągu znaków, wykonaj następujący kod:

```Ruby
string = "123abc456def"
new_string = string.gsub(/\d/, '')
puts new_string # wyświetli "abc def"
```

W powyższym przypadku, użyliśmy wyrażenia regularnego `/d/`, które pasuje do każdej cyfry. Aby usunąć inne znaki, możemy zmienić to wyrażenie na odpowiedni dla nas wzorzec. Na przykład, jeśli chcemy usunąć wszystkie znaki specjalne, możemy użyć `/[[:punct:]]/`.

```Ruby
string = "Hello! Are you there?"
new_string = string.gsub(/[[:punct:]]/, '')
puts new_string # wyświetli "Hello Are you there"
```

## Deep Dive

Metoda `gsub` jest odpowiednikiem metody `sub`, która usuwa tylko pierwsze wystąpienie danego znaku. Jednak, używając `gsub` z wyrażeniem regularnym, możemy usunąć wszystkie pasujące znaki. Przykładowo `/s/` pasuje do wszystkich białych znaków, więc możemy użyć `string.gsub(/\s/, '')` aby usunąć wszystkie białe znaki z tekstu.

Ważne jest również, aby pamiętać, że metoda `gsub` zwraca nowy zmodyfikowany ciąg znaków oraz nie zmienia oryginalnego. Jeśli chcemy nadpisać oryginalny string, musimy przypisać wynik metody `gsub` do tej samej zmiennej, jak w powyższych przykładach.

# Zobacz również

- [Ruby String Class](https://ruby-doc.org/core-2.6.3/String.html)
- [Ruby Regular Expressions](https://ruby-doc.org/core-2.6.3/Regexp.html)
- [Ruby Methods: gsub vs. sub](https://medium.com/@sbespalko/ruby-methods-gsub-vs-sub-f8b0adefc67a)