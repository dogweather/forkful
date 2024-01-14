---
title:                "Ruby: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego?

Tworzenie plików tymczasowych jest niezbędne w wielu przypadkach, szczególnie w programowaniu w języku Ruby. Często potrzebujemy przechowywać pewne dane, które są potrzebne tylko przez krótki czas i nie chcemy zapełniać naszego dysku stałego niepotrzebnymi plikami. Tworzenie plików tymczasowych jest idealnym rozwiązaniem w takich sytuacjach.

## Jak to zrobić?

Tworzenie plików tymczasowych w języku Ruby jest bardzo łatwe. Możemy to zrobić za pomocą metody "Tempfile.new" i przekazując jej jako argument prefiks nazwy pliku. Oto przykładowy kod:

```Ruby
require 'tempfile'
temp_file = Tempfile.new('my_temp_file')
puts temp_file.path
```

Wykonanie tego kodu spowoduje utworzenie pliku "my_temp_file" z dodanym losowym ciągiem znaków na końcu nazwy. Warto zauważyć, że plik ten zostanie automatycznie usunięty po zakończeniu działania programu.

## Wnikliwe spojrzenie

Inną przydatną funkcją w języku Ruby jest możliwość zmiany domyślnego zakresu, w którym plik tymczasowy jest tworzony i przechowywany. Dzięki temu możemy określić, gdzie chcemy, żeby plik tymczasowy był zapisany. Możemy to zrobić za pomocą opcji "dir" i "tmpdir". Oto przykładowy kod:

```Ruby
require 'tempfile'
temp_file = Tempfile.new('my_temp_file', 'tmpdir', dir: '/home/user/desktop')
puts temp_file.path
```

W tym przypadku, plik tymczasowy zostanie zapisany na pulpicie użytkownika.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o tworzeniu plików tymczasowych w języku Ruby, możesz przeczytać następujące artykuły:
- [Working with temporary files in Ruby](https://blog.logrocket.com/working-with-temporary-files-in-ruby/)
- [How to use Tempfile in Ruby](https://www.rubyguides.com/2015/06/ruby-temporary-file/)