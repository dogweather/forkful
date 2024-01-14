---
title:                "Ruby: Czytanie argumentów wiersza poleceń"
simple_title:         "Czytanie argumentów wiersza poleceń"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Witajcie czytelnicy! W dzisiejszym poście porozmawiamy o tym, dlaczego warto nauczyć się czytać argumenty wiersza poleceń w języku Ruby. Może wydawać się to na pierwszy rzut oka dość nieistotne, ale prawda jest taka, że umiejętność czytania argumentów wiersza poleceń jest niezwykle przydatna w pracy programisty.

## Jak to zrobić

Aby czytać argumenty wiersza poleceń w Ruby, musimy skorzystać z powłoki ARGV. Jest to wbudowany obiekt, który przechowuje argumenty podane przez użytkownika. Możemy do niego uzyskać dostęp poprzez wpisanie `$ARGV` w naszym skrypcie. Poniżej znajduje się przykładowy kod w języku Ruby:

```Ruby
# przykładowy program, który wczytuje argumenty wiersza poleceń
puts "Cześć #{ARGV[0]}, witaj w świecie Ruby!"
puts "Twój argument to: #{ARGV[1]}"

# przykładowe uruchomienie programu z argumentami wiersza poleceń
ruby program.rb Adam świetnie
```

Wynik działania powyższego programu będzie wyglądał następująco:

```
Cześć Adam, witaj w świecie Ruby!
Twój argument to: świetnie
```

## Głębszy zanurzenie

Przeczytanie argumentów wiersza poleceń jest szczególnie ważne, gdy nasz program wymaga podania jakichś danych lub opcji przy jego uruchomieniu. Dzięki temu możemy dostosować działanie naszego programu w zależności od podanych przez użytkownika argumentów. Ważnym aspektem jest również poprawna obsługa możliwych błędów, na przykład gdy użytkownik nie poda wymaganych argumentów. Warto również pamiętać o różnicach w interpretacji argumentów przez różne systemy operacyjne.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej na temat czytania argumentów wiersza poleceń w języku Ruby, zerknij na poniższe linki:

- [Dokumentacja Ruby - ARGV](https://ruby-doc.org/core-2.7.2/ARGF.html)
- [Poradnik dla początkujących programistów w Ruby](https://blog.appsignal.com/2021/04/21/ruby-for-beginners-reading-command-line-arguments.html)
- [Video tutorial - czytanie argumentów wiersza poleceń](https://www.youtube.com/watch?v=dQw4w9WgXcQ) (żartujemy, ale na pewno znajdziesz wiele przydatnych tutoriali na YouTube!)