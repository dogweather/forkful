---
title:    "Ruby: Odczytywanie pliku tekstowego"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów zastanawia się, dlaczego warto czytać pliki tekstowe w języku Ruby. Niektórzy mogą sądzić, że jest to mało przydatne, ponieważ większość danych jest przechowywana w bazach danych lub innych formach strukturalnych. Jednak czytanie plików tekstowych jest niezwykle ważne w wielu przypadkach, szczególnie w przypadku przetwarzania dużych ilości danych lub wykonywania automatycznych zadań. W tym artykule omówimy, jak czytać pliki tekstowe w języku Ruby i dlaczego jest to ważne dla każdego programisty.

## Jak to zrobić

Można czytać pliki tekstowe w języku Ruby za pomocą wbudowanych funkcji, takich jak `File.readlines()` lub `File.foreach()`. Pierwsza metoda odczytuje całą zawartość pliku na raz i zwraca ją jako tablicę, w której każdy wiersz jest osobnym elementem. Druga metoda iteruje po pliku, odczytując go linia po linii. Aby użyć tych funkcji, należy podać ścieżkę do pliku jako argument.

```Ruby
lines = File.readlines("plik.txt")
lines.each do |line|
  puts line
end
```

W powyższym przykładzie użyliśmy metody `each` do wyświetlenia odczytanego tekstu. Można również przypisać go do zmiennej i wykorzystać go w innych częściach kodu.

## W głębsze szczegóły

W języku Ruby istnieje wiele innych metod i narzędzi, które są przydatne przy czytaniu plików tekstowych. Na przykład, używając klasy `File`, można w prosty sposób sprawdzić, czy dany plik istnieje lub uzyskać informacje na temat jego rozmiaru i czasu utworzenia. Można również użyć wyrażeń regularnych do przetwarzania tekstu odczytanego z pliku i wyodrębnić potrzebne informacje.

Czytanie plików tekstowych jest również niezbędne w przypadku przetwarzania danych z zewnętrznych źródeł lub wykonywania operacji na plikach zapisanych przez inne programy. W języku Ruby można również pisać skrypty, które będą czytać i przetwarzać pliki tekstowe automatycznie, co może zaoszczędzić wiele czasu i wysiłku.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o czytaniu plików tekstowych w języku Ruby, zajrzyj na poniższe strony:

- [Dokumentacja Ruby o klasie File](https://ruby-doc.org/core/File.html)
- [Nauka Ruby: Czytanie plików](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-control-flow-u/cheatsheet)
- [Przewodnik Ruby dla programistów: Pliki i system operacyjny](http://www.rubykoans.com/)