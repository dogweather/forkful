---
title:    "Elixir: Ekstrakcja podłańcuchów"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyodrębnianie podłańcuchów jest ważnym narzędziem w programowaniu Elixir. Pozwala ono na pracę z konkretnymi częściami ciągów znaków, co jest szczególnie przydatne przy przetwarzaniu danych. W tym artykule opiszemy, dlaczego warto nauczyć się wyodrębniać podłańcuchy i jak to zrobić.

## Jak to zrobić

Wyodrębnianie podłańcuchów jest proste i intuicyjne w Elixirze. Możemy użyć funkcji `String.slice/3` wraz z indeksami początkowym i końcowym, aby wyodrębnić podłańcuch z danego ciągu znaków. Spójrzmy na przykładowy kod:

```Elixir
string = "Programowanie jest super zabawne"

slice = String.slice(string, 0, 11)

IO.puts("Wyodrębniony podłańcuch: #{slice}")

```

Powyższe polecenie wyświetli "Wyodrębniony podłańcuch: Programowanie".

Możemy również wyodrębniać podłańcuchy na podstawie znaków zamiast indeksów. W tym celu możemy użyć funkcji `String.split/2`, która dzieli ciąg znaków na mniejsze podłańcuchy na podstawie określonego znaku lub wyrażenia regularnego. Na przykład:

```Elixir
string = "Mam jutro egzamin z Elixira"

words = String.split(string, " ")

IO.inspect(words)

```

Powyższe polecenie wyświetli listę słów: ["Mam", "jutro", "egzamin", "z", "Elixira"].

## Deep Dive

W Elixirze istnieją również inne funkcje, które pozwalają na wyodrębnianie podłańcuchów, takie jak `String.joins/2`, `String.replace/3` czy `String.trim/2`. Możemy również użyć wyrażeń regularnych, aby dopasować i wyodrębnić określone wzorce z ciągu znaków.

Ważne jest również zwrócenie uwagi na wydajność operacji wyodrębniania podłańcuchów. Ze względu na to, że w Elixirze ciągi znaków są niemodyfikowalne, każda operacja wyodrębniania podłańcuchu tworzy nowy ciąg, co może być kosztowne dla dużej ilości danych.

## Zobacz też

- [Dokumentacja Elixir do funkcji String](https://hexdocs.pm/elixir/String.html)
- [Poradnik programowania w Elixirze](https://www.tutorialspoint.com/elixir/index.htm)
- [Github - przykładowe projekty w Elixirze](https://github.com/h4cc/awesome-elixir)