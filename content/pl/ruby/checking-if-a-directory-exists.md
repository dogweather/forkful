---
title:                "Sprawdzanie, czy istnieje katalog."
html_title:           "Ruby: Sprawdzanie, czy istnieje katalog."
simple_title:         "Sprawdzanie, czy istnieje katalog."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co & dlaczego?

Sprawdzanie czy istnieje katalog to po prostu wykonywanie określonej operacji w celu stwierdzenia czy określony katalog istnieje w systemie plików czy nie. Programiści często wykonują tę czynność, ponieważ jest to niezbędne w wielu aplikacjach, takich jak tworzenie lub usuwanie plików, zarządzanie uprawnieniami, lub po prostu sprawdzenie, czy dana lokalizacja jest dostępna do zapisu lub odczytu.

## Jak to zrobić:

Sprawdzenie czy katalog istnieje w Ruby jest proste i można to zrobić na kilka sposobów, na przykład używając metody `File.exist?` lub `Dir.exist?`. Poniżej znajdują się przykładowe kody wraz z wyjściami dla tych metod:

```Ruby
# Przykład sprawdzania czy katalog istnieje za pomocą metody File.exist?
File.exist?("folder")
=> false

# Przykład sprawdzania czy katalog istnieje za pomocą metody Dir.exist?
Dir.exist?("folder")
=> true
```

## Głębszy zanurzenie:

Sprawdzanie czy directory istnieje nie jest nową koncepcją wśród programistów. Wcześniej, w językach programowania, zamiast wykorzystywania gotowych metod, musieliśmy samodzielnie napisać kod, który wykonywałby tę operację. W Ruby, oprócz wspomnianych wcześniej metod, możemy również użyć metody `File.directory?`, która zwraca `true` tylko wtedy, gdy plik jest katalogiem. Alternatywnym sposobem jest użycie metody `Dir.entries`, która zwraca listę wszystkich plików i katalogów w danym katalogu.

## Zobacz również:

- [Ruby Dokumentacja o metodzie File.exist?](https://ruby-doc.org/core-2.7.1/File.html#method-c-exist-3F)
- [Ruby Dokumentacja o metodzie Dir.exist?](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F)
- [Ruby Dokumentacja o metodzie File.directory?](https://ruby-doc.org/core-2.7.1/File.html#method-i-directory-3F)
- [Ruby Dokumentacja o metodzie Dir.entries](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-entries)