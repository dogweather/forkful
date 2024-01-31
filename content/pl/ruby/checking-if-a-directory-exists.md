---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:58:22.852351-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Co & Dlaczego?)
Sprawdzamy, czy katalog istnieje, aby uniknąć błędów podczas próby dostępu czy też zapisu. Dzięki temu możemy też dynamicznie zarządzać strukturą katalogów w naszej aplikacji.

## How to: (Jak to zrobić:)
Sprawdzanie, czy katalog istnieje w Ruby, jest proste. Oto jak:

```Ruby
require 'fileutils'

if Dir.exist?('/sciezka/do/twojego/katalogu')
  puts 'Katalog istnieje!'
else
  puts 'Katalog nie istnieje!'
end
```

Jeśli katalog istnieje, zobaczysz:
```
Katalog istnieje!
```

W przeciwnym razie:
```
Katalog nie istnieje!
```

## Deep Dive (Dogłębna analiza)
Metoda `Dir.exist?` pojawiła się w Ruby 1.9, zastępując starą metodę `File.exists?`, która też mogła sprawdzać katalogi. 

Alternatywą jest użycie `File.directory?`, która sprawdzi, czy dana ścieżka jest katalogiem:

```Ruby
if File.directory?('/sciezka/do/twojego/katalogu')
  puts 'To jest katalog!'
else
  puts 'To nie jest katalog lub nie istnieje!'
end
```

Ruby używa biblioteki `FileUtils` dla wielu operacji na plikach i katalogach. Jest ona bogata w metody, które mogą pomóc w znacznie bardziej zaawansowanym zarządzaniu plikami.

## See Also (Zobacz również)
Dokumentacja Ruby Dir class: https://ruby-doc.org/core-3.1.2/Dir.html
Dokumentacja Ruby FileUtils: https://ruby-doc.org/stdlib-3.1.2/libdoc/fileutils/rdoc/FileUtils.html
Ruby API (po angielsku): https://rubyapi.org/
