---
title:                "Pisanie do standardowego błędu"
date:                  2024-01-19
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"

category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Pisanie do standardowego błędu (stderr) pozwala segregować normalny output od komunikatów o błędach. Programiści robią to, by łatwo rozróżniać standardowe dane wyjściowe od informacji o wyjątkach i innych błędach.

## How to: (Jak to zrobić:)
```Ruby
# Wypisanie komunikatu na standardowe wyjście
puts "Hello, this is standard output."

# Wypisanie komunikatu na standardowe wyjście błędów
$stderr.puts "Warning, this is an error message!"

# Użycie STDERR jest równoznaczne z $stderr
STDERR.puts "ERROR: Something bad happened!"
```

Przykładowy output:
```
Hello, this is standard output.
```
Przykładowy standard error (stderr):
```
Warning, this is an error message!
ERROR: Something bad happened!
```

## Deep Dive (Wnikliwa analiza)
Historia: STDERR zostało stworzone jako jedno z trzech głównych strumieni danych w Unix, ułatwiających zarządzanie wyjściem procesów.
Alternatywy: Możesz przekierować stderr do pliku lub innego strumienia za pomocą IO, jak `$stderr.reopen('errors.log', 'w')`.
Implementacja: W Rubym, `STDERR` i `$stderr` to globalne zmienne reprezentujące standardowy strumień błędów. Z reguły `STDERR` jest to samo co `$stderr`, ale `$stderr` może zostać przekierowany.

## See Also (Zobacz też)
- Ruby Docs on I/O: [https://ruby-doc.org/core-2.7.0/IO.html](https://ruby-doc.org/core-2.7.0/IO.html)
- StackOverflow: ["How to redirect stdout and stderr to a file in Ruby?"](https://stackoverflow.com/questions/1154846/how-to-redirect-stdout-and-stderr-to-a-file-in-ruby)
- UNIX Stdout and Stderr: [https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))
