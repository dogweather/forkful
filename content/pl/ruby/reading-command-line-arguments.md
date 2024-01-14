---
title:                "Ruby: Odczytywanie argumentów z linii poleceń"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jednym z podstawowych zadań programisty jest umiejętne czytanie argumentów wiersza poleceń. Pozwala to na zapewnienie interaktywności i różnych opcji dla użytkowników naszych programów. W tym wpisie dowiecie się, dlaczego warto nauczyć się czytać argumenty wiersza poleceń oraz jak to zrobić w języku Ruby.

## Jak to zrobić

Do odczytywania argumentów wiersza poleceń w Ruby możemy wykorzystać wbudowaną w język klasę `ARGV`. Wystarczy dodać odpowiedni kod na początku naszego skryptu:

```Ruby
# Odczytanie argumentu wiersza poleceń
puts "Witaj, #{ARGV[0]}!"
```

Po uruchomieniu skryptu z argumentem `Ruby` otrzymamy wyjście `Witaj, Ruby!`. Warto również pamiętać, że indeksowanie w tablicach zaczyna się od 0, dlatego `ARGV[0]` odpowiada pierwszemu podanemu argumentowi.

Możemy również wykorzystać pętlę `each` do odczytania wszystkich argumentów:

```Ruby
# Odczytanie wszystkich argumentów wiersza poleceń
ARGV.each do |arg|
  puts "Cześć, #{arg}!"
end
```

Po uruchomieniu skryptu z argumentami `Ruby` i `programowanie` otrzymamy:

```
Cześć, Ruby!
Cześć, programowanie!
```

## Deep Dive

Klasa `ARGV` w Ruby zapewnia nam nie tylko możliwość odczytania argumentów, ale także ich modyfikacji. Możemy na przykład usunąć pierwszy argument z listy:

```Ruby
# Usunięcie pierwszego argumentu
ARGV.shift
```

Możemy również dodać nowe argumenty do listy:

```Ruby
# Dodanie nowego argumentu
ARGV << "dodatkowy argument"
```

Klasa `ARGV` również udostępnia funkcję `empty?`, która pozwala nam sprawdzić, czy lista argumentów jest pusta.

```
if ARGV.empty?
  puts "Nie podano żadnych argumentów!"
else
  puts "Liczba argumentów: #{ARGV.length}"
end
```

W celu odczytania argumentów zawierających spacje lub znaki specjalne, możemy użyć funkcji `split`:

```Ruby
# Odczytanie argumentów zawierających spacje
puts ARGV[1].split
```

Jeśli interesuje Cię bardziej zaawansowana manipulacja argumentami wiersza poleceń w Ruby, warto zapoznać się z dokumentacją [klasy `Shellwords`](https://ruby-doc.org/stdlib-2.7.2/libdoc/shellwords/rdoc/Shellwords.html).

## Zobacz również

- [Dokumentacja klasy `ARGV`](https://ruby-doc.org/core-2.7.2/ARGF.html)
- [Artykuł na temat odczytywania argumentów wiersza poleceń w Ruby](https://dev.to/ibraheem4/reading-command-line-arguments-in-ruby-2a7)