---
title:                "Analiza html"
html_title:           "Elixir: Analiza html"
simple_title:         "Analiza html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Parsing HTML jest niezbędnym narzędziem dla każdego programisty, który pracuje z danymi ze stron internetowych. Dzięki temu procesowi możliwe jest wyodrębnienie potrzebnych informacji i przetworzenie ich w bardziej czytelną i użyteczną formę.

## Jak to zrobić

Aby rozpocząć analizowanie HTML w języku Elixir, będziemy potrzebować jednego narzędzia - biblioteki Floki. Możemy ją zainstalować poprzez wykonanie poniższej komendy w terminalu:

```Elixir
mix escript.install hex floki
```

Następnie musimy zaimportować bibliotekę w naszym projekcie:

```Elixir
require Floki
```

Aby pobrać i analizować kod HTML, użyjemy funkcji `Floki.parse/1`:

```Elixir
html = """
<html>
  <head>
    <title>Przykładowa Strona</title>
  </head>
  <body>
    <h1>Witaj!</h1>
    <p>To jest przykładowa strona internetowa.</p>
    <ul>
      <li>Pierwszy element</li>
      <li>Drugi element</li>
      <li>Trzeci element</li>
    </ul>
  </body>
</html>
"""

Floki.parse(html)
```

Wykonanie powyższego kodu spowoduje zwrócenie reprezentacji drzewa DOM (Document Object Model). Teraz możemy wykorzystać różne funkcje z biblioteki Floki, aby wyodrębnić potrzebne nam informacje. Na przykład, aby pobrać tytuł strony, użyjemy funkcji `Floki.find/2`:

```Elixir
title = Floki.find(html, "title")
# zwraca element <title>Przykładowa Strona</title>
```

Aby wyodrębnić wszystkie elementy listy, użyjemy funkcji `Floki.find_all/2`:

```Elixir
list_items = Floki.find_all(html, "li")
# zwraca listę elementów <li>
```

## Pogłębiona analiza

Parsing HTML jest zdecydowanie szczegółowym i zaawansowanym procesem, a biblioteka Floki oferuje wiele funkcji, które pomagają w jego dokładnym przetwarzaniu. Na przykład, możemy wykorzystać selektory CSS do precyzyjnego wyodrębniania potrzebnych nam elementów:

```Elixir
Floki.find(html, "body h1")
# zwraca element <h1>Witaj!</h1>
```

Istnieją również funkcje do zmiany drzewa DOM i porównywania go z innymi drzewami. Warto zapoznać się z dokumentacją biblioteki Floki oraz eksperymentować z różnymi funkcjami, aby lepiej zrozumieć proces parsingu HTML.

## Zobacz także

- Dokumentacja biblioteki Floki: https://hexdocs.pm/floki/readme.html
- Przewodnik po analizowaniu HTML w Elixir: https://dev.to/iansinnott/parsing-html-in-elixir-with-floki-23p4