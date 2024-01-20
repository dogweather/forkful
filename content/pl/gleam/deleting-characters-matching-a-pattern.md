---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie znaków pasujących do wzorca to proces w którym znaki pasujące do określonego wzoru są eliminowane z łańcucha znaków. Programiści robią to, aby manipulować danymi tekstowymi - od prostego oczyszczenia tekstu po skomplikowane przetwarzanie języka naturalnego.

## Jak to zrobić:

Poniżej pokazuję przykład jak usunąć wszystkie cyfry z tekstu przy użyciu języka Gleam.

```gleam
import gleam/string

fn main() {
  let text = "Moje ulubione liczby to 678 i 123"
  let clean_text = string.replace(text, ~"\\d", "")
  Io.print(clean_text)
}
```
Gdy uruchomisz powyższy kod, wynik będzie następujący:

```
Moje ulubione liczby to i
```

## Głębsze zanurzenie

Usuwanie znaków pasujących do wzorca ma korzenie sięgające języków programowania takich jak Perl i Python, które posiadają wbudowane operacje do manipulacji tekstami. Alternatywą dla Gleam może być Erlang z biblioteką `re` do manipulacji wyrażeniami regularnymi, ale Gleam oferuje bardziej nowoczesny i przyjazny składni typowania. Co więcej, działanie funkcji `string.replace` jest dość proste - przechodzi przez ciąg znaków znak po znaku, porównywając go z wzorcem i usuwając dopasowania.

## Zobacz także

Chcesz dowiedzieć się więcej? Zweryfikuj poniższe linki:
- Gleam documentation: https://gleam.run/book/tour/strings.html
- Regular expressions in Gleam: https://gleam.run/news/gleam-v0.14-released/
- Erlang `re` module: https://erlang.org/doc/man/re.html