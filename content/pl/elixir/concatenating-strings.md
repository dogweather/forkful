---
title:                "Łączenie ciągów znaków"
html_title:           "Elixir: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# Co & Dlaczego?

Kontatenacja jest procesem łączenia ciągów znaków, czyli tekstowych danych, w jeden ciąg znaków. Programiści wykorzystują ten proces, aby tworzyć bardziej złożone teksty i wyświetlać je użytkownikom w czytelnej formie.

# Jak to zrobić?

```Elixir
"Hello " <> "world"
```
```console
Hello world
```

W języku Elixir używa się operatora <> do konkatenacji ciągów znaków. W powyższym przykładzie, ciągi "Hello" i "world" są połączone w jeden - "Hello world".

# Głębsze spojrzenie

Kontatenacja jest powszechnie używaną metodą w programowaniu. W przeszłości, w starszych językach programowania, konieczne było używanie specjalnych funkcji do łączenia ciągów. Jednak w dzisiejszych językach, takich jak Elixir, konkatenacja jest wbudowanym elementem składni.

Alternatywą dla kontatenacji jest użycie funkcji ```IO.puts```, która może przyjąć wiele argumentów i wyświetlić je w konsoli.

W języku Elixir, konkatenacja jest wydajniejszym rozwiązaniem do łączenia ciągów, ponieważ nie tworzy ona nowego obiektu, tylko łączy dwa istniejące.

# Zobacz także

Więcej informacji o kontatenacji w języku Elixir można znaleźć w oficjalnej dokumentacji: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#string-interpolation-and-concatenation

Inne ciekawe artykuły na temat języka Elixir można znaleźć na stronie Medium: https://medium.com/tag/elixir