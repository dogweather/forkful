---
title:                "Usuwanie cudzysłowów z ciągu znaków"
date:                  2024-01-26T03:39:20.120500-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usuwanie cudzysłowów z ciągu znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Usuwanie cudzysłowów ze stringa oznacza pozbycie się tych dodatkowych warstw – znaków cudzysłowu – z Twoich danych tekstowych. Programiści robią to w celu oczyszczenia danych wejściowych, przygotowania stringów do przetwarzania lub po prostu, aby utrzymać porządek i spójność w swoich aplikacjach. Chodzi o to, aby na końcu otrzymać czyste, użyteczne dane.

## Jak to zrobić:
Usunięcie cudzysłowów w Gleam jest proste. Możemy użyć dopasowania wzorców lub wbudowanych funkcji stringowych. Oto szybki przykład ilustrujący jak to zrobić:

```gleam
pub fn remove_quotes(text: String) -> String {
  let without_quotes = string.trim(text, "\"")
  without_quotes
}

pub fn main() {
  let text_with_quotes = "\"Hello, World!\""
  let cleaned_text = remove_quotes(text_with_quotes)
  io.println(cleaned_text)
}
```

Przykładowe wyjście:
```
Hello, World!
```

## Szczegółowe omówienie
Historycznie, radzenie sobie z cudzysłowami w stringach było powszechnym zadaniem w językach przetwarzania tekstu i skryptowych. Ze względu na to, że stringi często są danymi wejściowymi użytkownika lub odczytywane z plików, mogą zawierać znaki cudzysłowu, które należy usunąć z różnych powodów, takich jak wstawianie do bazy danych czy formatowanie.

W Gleam używamy funkcji `string.trim`, aby usunąć cudzysłowy. Istnieją alternatywy! Moglibyśmy przeglądać string lub stosować wyrażenia regularne, ale `string.trim` jest Twoim wygodnym narzędziem do pracy ze względu na jego zwięzłość i wydajność.

Gdy zagłębimy się w szczegóły implementacji, funkcja `string.trim` działa poprzez usuwanie znaków z początku i końca stringu, które pasują do podanego wzorca. Więc jeśli masz cudzysłowy na obu końcach swojego stringa, będą one odcięte za jednym zamachem. Pamiętaj, że usuwa cudzysłowy tylko jeśli są one na krawędziach; cudzysłowy siedzące wygodnie w środku tekstu pozostaną na miejscu.

## Zobacz również
Dla ciekawych umysłów, które chcą zbadać więcej:
- [Dokumentacja modułu String Gleam](https://gleam.run/stdlib/string/)
- Dyskusje na temat przetwarzania tekstu w programowaniu na [Stack Overflow](https://stackoverflow.com/questions/tagged/text-processing)
