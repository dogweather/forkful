---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyszukiwanie i zamiana tekstu to podstawowe operacje, które programiści wykonują na ciągach znaków. Znajdują one zastosowanie w wielu kontekstach, np. podczas manipulowania danymi, czyszczenia i normalizacji informacji.

## Jak to zrobić:

```Elixir 
original_text = "Witam wszystkich w Elixir!"
new_text = String.replace(original_text, "wszystkich", "Was")

IO.puts new_text
# Outputs: "Witam Was w Elixir!"
```

Kod powyżej pokazuje, jak użyć funkcji `replace` z modułu `String` do wyszukania i zamiany słowa 'wszystkich' na 'Was' w tekście "Witam wszystkich w Elixir!".

```Elixir 
pattern = ~r/wszystkich/i
replacement = "Was"
new_text = Regex.replace(pattern, original_text, replacement)

IO.puts new_text
# Outputs: "Witam Was w Elixir!"
```

Drugi kod prezentuje alternatywny sposób używania eksperymentalnej składy `~r` do tworzenia wyrażeń regularnych.

## W głąb tematu:

W poprzednich wersjach Elixir metoda 'replace' nie była dostępna. Programiści musieli korzystać z niskopoziomowych funkcji na ciągach znaków lub korzystać z bibliotek firm trzecich.

Metoda 'replace' z modułu 'String' jest prostsza do zrozumienia i stosowania, ale metoda z wyrażeniem regularnym jest bardziej wszechstronna. Może obsłużyć bardziej złożone przypadki, takie jak ignorowanie wielkości liter.

Rozważając, które podejście wybrać, należy wziąć pod uwagę specyfikacje projektu. Na przykład, jeśli robisz dużo złożonych manipulacji na tekstach, warto znać i używać wyrażeń regularnych.

## Zobacz też:

1. [Oficjalna dokumentacja Elixir na temat modułu String](https://elixir-lang.org/getting-started/basic-types.html#strings)
2. [Poradnik dotyczący składni wyrażeń regularnych w Elixir](https://elixir-lang.org/getting-started/pattern-matching.html#the-pin-operator)
3. [Dokumentacja dla `~r` składni](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%7Er/2)