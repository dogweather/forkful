---
title:                "Usuwanie znaków odpowiadających wzorcowi"
html_title:           "Gleam: Usuwanie znaków odpowiadających wzorcowi"
simple_title:         "Usuwanie znaków odpowiadających wzorcowi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Cześć programiści,

Dziś mówimy o tym, jak usuwać znaki pasujące do wzorca w języku programowania Gleam. W tym artykule dowiesz się, czym jest to operacja i dlaczego programiści jej używają.

## Co i czemu?

Usuwanie znaków pasujących do wzorca to czynność polegająca na usuwaniu określonych znaków z tekstu, które odpowiadają zadanemu wzorcowi. Jest to użyteczne w wielu sytuacjach, na przykład w procesowaniu danych lub weryfikacji użytkownika.

## Jak to zrobić?

Oto przykład kodu w języku Gleam, który usuwa wszystkie litery "a" z podanego tekstu:

```Gleam
lib String = import gleam/string

pub fn delete_a(text: String) String {
  replace(text, "a", "")
}

test "deleting a" {
  expect(delete_a("apple")).toBe("pple")
}
```
W powyższym przykładzie wykorzystujemy funkcję `replace` do zastąpienia wszystkich wystąpień liter "a" pustym ciągiem. Następnie testujemy funkcję, aby upewnić się, że działa prawidłowo.

## Głębszy zanurzenie

Jeśli jesteś ciekawy, skąd wzięła się operacja usuwania znaków pasujących do wzorca, warto przejrzeć historię wyrażeń regularnych. Wyrażenia regularne, czyli wzorce, zostały wprowadzone w 1958 roku przez Stephena Cole Kleena i od tego czasu są nieodłączną częścią wielu języków programowania. W Gleam, operacja usuwania znaków pasujących do wzorca jest również możliwa dzięki wyrażeniom regularnym.

Alternatywą dla usuwania znaków pasujących do wzorca jest użycie funkcji `filter`, która wyrzuci wszystkie znaki niepasujące do zadanego wzorca. Jednak w przypadku, gdy chcemy dokładnie określić, które znaki chcemy usunąć, operacja usuwania jest bardziej odpowiednia.

W języku Gleam, usuwanie znaków pasujących do wzorca jest zaimplementowane za pomocą funkcji `replace` z pakietu `gleam/string`. Funkcja ta przyjmuje dwa argumenty - tekst i wzorzec do usunięcia - i zwraca zmodyfikowany tekst.

## Zobacz także

Szukasz więcej informacji na temat języka Gleam i jego funkcji? Oto kilka przydatnych źródeł:

- Dokumentacja języka Gleam: https://gleam.run/documentation/
- Gleam w Github: https://github.com/gleam-lang/gleam

Zapraszam również na nasz blog, gdzie znajdziesz wiele ciekawych artykułów na temat programowania.

Dziękuję za przeczytanie tego artykułu. Mam nadzieję, że dowiedziałeś się czegoś nowego na temat usuwania znaków pasujących do wzorca w języku Gleam. Do zobaczenia w następnym artykule!