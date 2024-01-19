---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Lua: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzenie, czy dany katalog istnieje, polega na kode potwierdzenia obecności określonego katalogu w systemie plików. Programiści robią to, aby zapobiec błędom podczas pracy z plikami, takimi jak próba odczytu czy zapisu do nieistniejącego katalogu.

## Jak to zrobić:
Możesz sprawdzić, czy katalog istnieje, używając metody `directory?` z modułu `File`. Oto jak to zrobić:

```Ruby
if File.directory?("/ścieżka/do/katalogu")
  puts "Katalog istnieje."
else
  puts "Katalog nie istnieje."
end
```

Jeśli katalog istnieje, wygenerowany zostanie output:

```
Katalog istnieje.
```

Jeśli katalog nie istnieje, otrzymasz:
```
Katalog nie istnieje.
```

## Poruszam głębiej:
Kontrola, czy katalog istnieje, jest koniecznością od początków programowania. We wczesnych wersjach języka Ruby, alternatywą dla sprawdzenia, czy katalog istnieje, było łapanie wyjątku generowanego podczas próby odczytu lub zapisu do nieistniejącego katalogu. Konieczność ręcznego reagowania na te wyjątki, skłoniła twórców Ruby do dodania metody 'directory?'.

Nie ma jednej uniwersalnej metody kontroli, która będzie działać na wszystkich systemach operacyjnych. Ruby jednakowo dobrze poradzi sobie zarówno na Windowsie, jak i na Uniksie, używając właściwych do danej platformy funkcji systemowych.

## Zobacz także:
Więcej informacji na temat modułu `File` oraz jego metod znajdziesz tutaj:

1. [Dokumentacja Ruby - Moduł File](https://ruby-doc.org/core/File.html)
2. [StackOverflow - Jak sprawdzić, czy katalog istnieje w Ruby](https://stackoverflow.com/questions/753918/how-to-check-if-a-directory-exists-in-ruby)