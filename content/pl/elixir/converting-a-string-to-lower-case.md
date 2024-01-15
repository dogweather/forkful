---
title:                "Konwersja ciągu znaków do małych liter"
html_title:           "Elixir: Konwersja ciągu znaków do małych liter"
simple_title:         "Konwersja ciągu znaków do małych liter"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego?

Jeśli jesteś programistą Elixir i często pracujesz z tekstami, na pewno zdarzyło Ci się potrzebować przetworzyć ciąg znaków na małe litery. Dlaczego? Ponieważ jest to powszechnie stosowane przy sprawdzaniu czy dwa ciągi znaków są sobie równe, bez względu na wielkość liter. W tym artykule pokażę Ci, jak w prosty sposób przekonwertować string na małe litery w Elixir.

## Jak to zrobić?

Przetworzenie ciągu znaków na małe litery w Elixir jest bardzo łatwe i wymaga użycia wbudowanej funkcji ```String.downcase```, która jako argument przyjmuje nasz ciąg znaków.

```Elixir
String.downcase("HELLO WORLD") 
# output: "hello world"
```

Jeśli zamiast ciągu znaków, chcesz przekonwertować całe zdanie lub akapit, możesz użyć funkcji ```String.downcase/1``` w celu zastosowania jej do każdego słowa w zdaniu.

```Elixir
String.downcase("Elixir jest niesamowity!") 
# output: "elixir jest niesamowity!"
```

Funkcja ```String.downcase/1``` nie tylko przekonwertuje wszystkie znaki na małe litery, ale także usunie wszelkie diakrytyki czyli znaki specjalne stosowane w niektórych językach (np. polskich) do oznaczenia dźwięczności i bezdźwięczności.

```Elixir
String.downcase("Żywiec") 
# output: "żywiec"
```

## Zagłębienie

Funkcja ```String.downcase/1``` wewnętrznie korzysta z funkcji ```Enum.map/2```, co oznacza, że dla dużych ciągów znaków może być nieco mniej wydajna. W takich przypadkach lepszym rozwiązaniem może być użycie wbudowanej funkcji ```String.replace/4```, która działa na zasadzie zastępowania poszczególnych znaków w ciągu.

```Elixir
String.replace("HELLO WORLD", ~r/[A-Z]/, &String.downcase(&1))
# output: "hello world"
```

Kluczem do zrozumienia tego sposobu jest wykorzystanie regularnych wyrażeń, w tym przypadku ```~r/[A-Z]/``` oznacza, że będzie szukać wszystkich dużych liter. Następnie używamy funkcji ```&String.downcase/1``` jako argumentu w funkcji ```String.replace/4```, co oznacza, że dla każdego znalezionego wystąpienia dużej litery, zostanie użyta funkcja ```String.downcase/1``` w celu jej zamiany na małą literę.

## Zobacz także

Jeśli jesteś zainteresowany innymi funkcjami związanymi z przetwarzaniem tekstów w Elixir, polecam zapoznać się z dokumentacją wbudowanych funkcji, takich jak ```String.upcase/1``` czy ```String.capitalize/1```.

- https://hexdocs.pm/elixir/String.html#downcase/1
- https://hexdocs.pm/elixir/String.html#downcase/2
- https://hexdocs.pm/elixir/String.html#replace/4