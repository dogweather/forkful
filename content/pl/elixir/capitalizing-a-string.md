---
title:    "Elixir: Konwersja na wielkie litery w ciągu znaków"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego warto użyć Elixira do kapitalizacji ciągów znaków?

Kapitalizacja, czyli zamiana pierwszej litery w tekście na wielką, jest jedną z podstawowych operacji w programowaniu. Często musimy wykonać tę czynność przy przetwarzaniu danych wejściowych, na przykład w formularzach internetowych lub przy tworzeniu raportów. Dlatego warto poznać narzędzia, które umożliwiają nam szybkie i efektywne wykonywanie takich operacji, a jednym z nich jest Elixir.

## Jak to zrobić?

Kodowanie z Elixirem jest bardzo intuicyjne, dlatego nauka kapitalizacji ciągów znaków w tym języku jest łatwa i przyjemna. Wystarczy wykorzystać funkcję `String.capitalize/1`, która przyjmuje jako argument ciąg znaków i zwraca ten sam ciąg, ale z pierwszą literą zamienioną na wielką. Możemy również wykorzystać operator `^` do łączenia funkcji `String.capitalize/1` z innymi funkcjami, co zapewnia większą elastyczność i czytelność kodu.

```Elixir
# Przykładowe dane wejściowe
input = "elixir jest niesamowitym językiem!"
# Wykorzystanie funkcji String.capitalize/1
String.capitalize(input)
# Wynik: "Elixir jest niesamowitym językiem!"
# Wykorzystanie operatora ^
input |> String.capitalize() |> String.replace("niesamowitym", "fantastycznym")
# Wynik: "Elixir jest fantastycznym językiem!"
```

## Głębszy wgląd w kapitalizację ciągów znaków

W Elixirze istnieje również funkcja `String.capitalize/2`, która dodatkowo przyjmuje opcjonalny argument `:ascii` lub `:unicode`, określając sposób kapitalizacji w zależności od rodzaju znaków wejściowych. Dodatkowo, w języku istnieje wiele innych funkcji i sposobów manipulacji ciągami znaków, na przykład `String.upcase/1` czy `String.downcase/1`, co daje większe możliwości w tworzeniu różnego rodzaju aplikacji.

## Zobacz również

- Dokumentacja Elixir - https://hexdocs.pm/elixir/String.html
- Przykładowe operacje na ciągach znaków w Elixirze - https://www.youtube.com/watch?v=IgzhvaiOGBw
- 10 powodów, dla których warto nauczyć się Elixira - https://blog.appsignal.com/2018/02/27/10-reasons-to-learn-elixir.html