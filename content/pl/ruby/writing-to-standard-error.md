---
title:                "Ruby: Pisanie do standardowego błędu."
simple_title:         "Pisanie do standardowego błędu."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu jest nieodłączną częścią pisania kodu w Ruby. Wiele osób może zastanawiać się dlaczego jest to ważne lub dlaczego w ogóle powinniśmy to robić. W tym wpisie dowiesz się, dlaczego pisanie do standardowego błędu jest tak istotne i jak możesz to zrobić.

## Jak to zrobić

Aby napisać do standardowego błędu w Ruby, możesz użyć metody `warn` lub `stderr.puts`. Możesz także przekierować standardowe wyjście do standardowego błędu za pomocą operatora `>>`. Przykładowy kod wyglądałby następująco:

```Ruby
# Metoda warn
warn "Podany argument jest nieprawidłowy"

# Metoda stderr.puts
$stderr.puts "Wystąpił błąd podczas wykonania programu"

# Przekierowanie standardowego wyjścia do standardowego błędu
puts "Witaj na stronie internetowej" >> $stderr
```

Powyższy kod używa różnych sposobów, aby wypisać informację na standardowy błąd. Możesz wybrać ten, który najlepiej pasuje do twojego kodu i potrzeb.

## Deep Dive

Pisanie do standardowego błędu jest ważne, ponieważ pozwala nam na wyświetlenie informacji o błędach lub problemach występujących podczas wykonania programu. Standardowe wyjście jest używane do wyświetlania informacji wyjściowych dla użytkownika, a standardowy błąd do wyświetlania komunikatów o błędach lub problemach. Dzięki temu możemy lepiej kontrolować i zarządzać naszym kodem.

## Zobacz także

Zapoznaj się z poniższymi linkami, aby dowiedzieć się więcej o pisaniu do standardowego błędu w Ruby:

- [Dokumentacja Ruby](https://ruby-doc.org/core-2.7.2/Kernel.html#method-i-warn)
- [Wideo na YouTube](https://www.youtube.com/watch?v=7Cs7TDYhuZI)
- [Artykuł na Medium](https://medium.com/@josh_cheek/stderr-warn-puts-and-io-64b1224a5c1d)