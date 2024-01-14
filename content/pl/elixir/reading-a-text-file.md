---
title:    "Elixir: Czytanie pliku tekstowego"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się czytać pliki tekstowe w swoim kodzie Elixir? Jeśli nie, to może się zastanawiasz po co to robić. W tym artykule postaram się wyjaśnić dlaczego jest to ważna umiejętność, którą warto opanować.

## Jak to zrobić

Najprostszym sposobem na czytanie plików tekstowych w Elixir jest użycie funkcji `File.read/1`, która zwraca całą zawartość pliku jako napis. Przykład użycia tej funkcji wyglądałby następująco:

```Elixir
content = File.read("plik.txt")
IO.puts(content)
```

W powyższym przykładzie, zawartość pliku "plik.txt" zostanie wczytana i wypisana na ekranie. Możesz również skorzystać z funkcji `File.stream!/2`, która zwraca strumień zawierający linie tekstu z pliku. Przykład użycia tej funkcji wyglądałby tak:

```Elixir
File.stream!("plik.txt")
|> Enum.each(&(IO.puts &1))
```

Ten kod iteruje przez każdą linię tekstu z pliku i wypisuje ją na ekranie. W ten sposób można wygodnie przetwarzać pliki tekstowe w Elixir.

## Głębsza analiza

Czytanie plików tekstowych może być bardzo przydatne w wielu sytuacjach. Możesz użyć tego do przetwarzania danych zapisanych w plikach, takich jak pliki CSV, pliki konfiguracyjne lub nawet pliki logów. W Elixir, czytanie plików jest szybkie i wydajne, więc jest to dobra opcja, jeśli potrzebujesz przetworzyć dużą ilość danych.

## Zobacz również

Jeśli jesteś zainteresowany dalszym zgłębianiem tematu czytania plików tekstowych w Elixir, polecam zapoznać się z dokumentacją [File](https://hexdocs.pm/elixir/File.html) oraz [Enum](https://hexdocs.pm/elixir/Enum.html). Możesz również sprawdzić inne artykuły na temat Elixira, takie jak ["Elixir: podstawowe konstrukcje języka"](https://bykowski.pl/elixir-podstawowe-konstrukcje-jezyka/) lub ["Pobieranie i zapisywanie danych w Elixir z użyciem GenStage"](https://lunarbeam.net/2017/09/01/elixir-parallel-sinks-sources-and-genstage/).