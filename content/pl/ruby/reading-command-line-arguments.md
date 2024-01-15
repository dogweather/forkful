---
title:                "Wczytywanie argumentów wiersza poleceń"
html_title:           "Ruby: Wczytywanie argumentów wiersza poleceń"
simple_title:         "Wczytywanie argumentów wiersza poleceń"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiałeś się kiedyś, jak programy przetwarzają dane, które dostają z linii poleceń? Czy kiedykolwiek chciałeś napisać bardziej interaktywny skrypt, który wykorzystuje argumenty z linii poleceń? Jeśli tak, ten artykuł jest dla Ciebie! 

##Jak To Zrobić

W Ruby istnieje prosty sposób na odczytywanie argumentów z linii poleceń. Potrzebujemy tylko jednej metody - `ARGV`. Jest to tablica, która przechowuje wszystkie argumenty przekazane z linii poleceń.

Proste wywołanie programu z argumentami może wyglądać tak:

```Ruby
ruby program.rb argument1 argument2 argument3
```

Aby odczytać te argumenty, wystarczy użyć pętli `each` i przeiterować przez tablicę `ARGV`:

```Ruby
ARGV.each do |arg|
    puts "Otrzymałem argument: #{arg}"
end
```

Output:

```
Otrzymałem argument: argument1
Otrzymałem argument: argument2
Otrzymałem argument: argument3
```

Możemy również sprawdzić ilość przekazanych argumentów, używając metody `length` na `ARGV`:

```Ruby
puts "Otrzymałem #{ARGV.length} argumentów" 
```

Output: 

```
Otrzymałem 3 argumentów
```

## Zagłębienie

Jeśli chcesz przejść głębiej i dowiedzieć się więcej o odczytywaniu argumentów z linii poleceń w Ruby, możesz przeczytać dokumentację [ARGV](https://ruby-doc.org/core-2.7.1/ARGF.html) oraz skorzystać z innych metod dostępnych dla tablic, takich jak `find`, `select` czy `include?`. 

## Zobacz również

Jeśli chcesz poszerzyć swoją wiedzę na temat programowania w Ruby, zapoznaj się z innymi artykułami dostępnymi na stronie Ruby's official documentation lub skorzystaj z darmowych tutoriali i zasobów na [Codeacademy](https://www.codecademy.com/learn/learn-ruby).