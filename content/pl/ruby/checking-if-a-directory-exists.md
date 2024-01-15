---
title:                "Sprawdzanie czy istnieje katalog"
html_title:           "Ruby: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzenie, czy katalog istnieje, jest ważnym elementem programowania w Ruby. Może to być przydatne na przykład podczas tworzenia aplikacji, która wymaga dostępu do określonego katalogu lub plików w nim zawartych.

## Jak to zrobić

Sprawdzenie istnienia katalogu w Ruby jest bardzo proste dzięki wbudowanej metodzie `Dir.exist?`. Poniższy kod pokazuje przykład użycia tej metody:

```Ruby
if Dir.exist?('folder')
  puts "Katalog istnieje"
else
  puts "Katalog nie istnieje"
end
```

Jeśli `Dir.exist?` zwróci wartość `true`, oznacza to, że katalog istnieje, w przeciwnym wypadku zostanie zwrócona wartość `false`.

## Deep Dive

Metoda `Dir.exist?` działa poprzez sprawdzenie, czy podany katalog istnieje w systemie plików. Jeśli podany katalog nie istnieje, metoda zwróci wartość `false`. Jeśli jednak katalog istnieje, metoda sprawdzi również, czy użytkownik ma odpowiednie uprawnienia do odczytu tego katalogu. Jeśli użytkownik nie ma takich uprawnień, metoda również zwróci wartość `false`.

Innym sposobem na sprawdzenie istnienia katalogu jest użycie metody `File.directory?`. Oba wyżej wymienione sposoby są równoważne i zwracają te same wyniki.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o jakichkolwiek innych metodach związanych ze sprawdzaniem katalogów w Ruby, możesz zapoznać się z dokumentacją Ruby lub przeczytać inne artykuły na ten temat, takie jak:

- [Ruby Docs on Dir Class](https://ruby-doc.org/core-2.7.1/Dir.html)
- [Ruby Docs on File Class](https://ruby-doc.org/core-2.7.1/File.html)