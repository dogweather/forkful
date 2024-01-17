---
title:                "Pobieranie strony internetowej"
html_title:           "Elixir: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

"## Co i dlaczego?"
Pobieranie strony internetowej to proces, w którym program pobiera zawartość strony internetowej i wyświetla ją użytkownikowi. Programiści często pobierają strony internetowe, aby analizować i przetwarzać dane lub wykorzystać je w swoich projektach.

"## Jak to zrobić:"
```Elixir
# Aby pobrać stronę internetową w Elixirze, należy użyć funkcji HTTPoison.get(), podając adres URL jako argument.
strona = HTTPoison.get("https://example.com")

# W celu uzyskania zawartości strony należy użyć funkcji HTTPoison.body() na zwróconym wyniku.
zawartosc = HTTPoison.body(strona)

# Następnie można wyświetlić pobraną zawartość za pomocą funkcji IO.puts().
IO.puts(zawartosc)

# Przykładowy wynik:
# <html>
#   <head>
#     <title>Przykładowa strona</title>
#   </head>
#   <body>
#     <h1>Witaj na przykładowej stronie!</h1>
#   </body>
# </html>
```

"## Deep Dive": 
W przeszłości pobieranie stron internetowych było skomplikowanym procesem, jednak dzięki nowoczesnym językom programowania i bibliotekom takim jak HTTPoison jest to teraz proste i szybkie. Istnieją również inne sposoby na pobieranie strony internetowej, takie jak używanie biblioteki zewnętrznej CURL lub wykorzystanie wbudowanych funkcji przeglądarki.

"## Zobacz też": 
- Dokumentacja HTTPoison: https://hexdocs.pm/httpoison/HTTPoison.html
- Biblioteka CURL dla Elixira: https://github.com/elixir-curl/curl