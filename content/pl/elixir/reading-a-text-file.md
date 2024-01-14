---
title:                "Elixir: Odczytywanie pliku tekstowego."
simple_title:         "Odczytywanie pliku tekstowego."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub chcesz stać się jednym, prawdopodobnie masz do czynienia z przetwarzaniem plików tekstowych w swojej pracy. Niezależnie od tego, czy jest to analiza danych czy przetwarzanie danych logów, odczytywanie plików tekstowych jest częstym zadaniem w pracy programisty. W tym wpisie dowiesz się, jak w języku Elixir można odczytać plik tekstowy i przetworzyć jego zawartość.

## Jak to zrobić

Pierwszym krokiem jest otwarcie pliku tekstowego w trybie tylko do odczytu przy użyciu funkcji `File.open/2`. Następnie możemy odczytać zawartość pliku przy użyciu metody `IO.read/2`, która zwraca całą zawartość pliku jako łańcuch znaków. Przykładowo:

```Elixir
file = File.open("plik.txt", [:read])
IO.read(file)
```

Jeśli chcemy odczytać plik linia po linii, możemy użyć funkcji `IO.stream/2`, która zwraca strumień zawierający kolejne linie pliku. Następnie możemy wykorzystać strumień w wyrażeniu `for` w celu przetworzenia zawartości pliku linia po linii. Przykładowo:

```Elixir
file = File.open("plik.txt", [:read])
stream = IO.stream(file, :line)
for line <- stream do
  IO.puts(line)
end
```

Jeśli wiesz, że plik tekstowy jest strukturalny, możesz skorzystać ze wbudowanych parserów Elixir, takich jak `CSV` lub `Jason`, aby przetworzyć go w bardziej czytelny sposób. Przykładowo:

```Elixir
file = File.open("plik.csv", [:read])
CSV.decode!(file)
```

## Głębsze zagłębienie

W języku Elixir można również wykorzystać bibliotekę `File` do operacji na plikach tekstowych. Warto zapoznać się z funkcjami takimi jak `File.stream!/3`, `File.read!/2` czy `File.write!/2`, które pozwalają na bardziej zaawansowane operacje na plikach.

Warto również wiedzieć, że w języku Elixir można wykorzystać mechanizm bloków (ang. pipelines) do przetwarzania danych tekstowych. Dzięki temu możemy efektywnie manipulować zawartością pliku tekstowego w przepływie danych, wykorzystując różnego rodzaju funkcje i operatory. Przykładowo:

```Elixir
File.stream!("plik.txt", [:line]) |> Enum.filter(fn line -> String.length(line) > 10 end) |> Enum.map(fn line -> String.upcase(line) end) |> Enum.each(fn line -> IO.puts(line) end)
```

## Zobacz również

* [Oficjalna dokumentacja Elixir - Praca z plikami](https://elixir-lang.org/getting-started/file.html)
* [Poradnik Elixir - Obsługa plików tekstowych](https://elixirschool.com/pl/lessons/basics/file/)
* [Elixir Forum - Przetwarzanie danych z pliku CSV](https://elixirforum.com/t/how-to-process-data-from-csv-file-using-elixir/10778) (dostępne tylko w języku angielskim)