---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Czym i Dlaczego? 
Zakładanie nowego projektu to tworzenie nowej pracy od zera. Programiści robią to, aby swobodnie pracować nad nowym pomysłem, innym podejściem lub rozwiązaniem problemu.

## Jak to zrobić:
Zacznijmy od instalacji Elixir. Można to zrobić, korzystając z poniższego polecenia dla systemów Unix/Linux:
```sh
wget https://repo.hex.pm/builds/elixir/v1.12.3.zip
unzip v1.12.3.zip
```

Tworzenie nowego projektu Elixir odbywa się poprzez narzędzie `mix`. Oto przykład:
```elixir
mix new moj_projekt
```
Wynik będzie wyglądał mniej więcej tak:
```sh
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating config
* creating config/config.exs
* creating lib
* creating lib/moj_projekt.ex
* creating test
* creating test/test_helper.exs
* creating test/moj_projekt_test.exs

Pomyślnie utworzono projekt.
``` 
## Głębokie Wnurzenie:
Elixir jest językiem programowania zaprojektowanym i rozwijanym przez José Valima, współzałożyciela platformy Phoenix. Dziedziczy wiele cech z Erlang, ale z bardziej przyjazną dla użytkowników składnią.

Alternatywy dla Elixir to Haskell, Scala, lub nawet Go, chociaż żaden z nich nie oferuje takiego samego poziomu skalowalności, niezawodności i na pewnym poziomie uproszczenia jak Elixir.

Głównym narzędziem do tworzenia nowego projektu w Elixir jest `mix`, który automatycznie generuje podstawową strukturę projektu. Pozwala to programistom skupić się na tworzeniu funkcji, zamiast budowaniu struktury od podstaw.

## Zobacz Też:
- Oficjalna dokumentacja Elixir: https://elixirschool.com/pl/
- Czytanie kodu Elixir: https://github.com/TheFirstAvenger/elixir-1
- Porównanie różnych języków programowania (w tym Elixir) do Erlang: https://www.erlang.org/elixir-for-erlang-programmers