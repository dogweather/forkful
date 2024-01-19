---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Sprawdzenie, czy katalog istnieje, to proces polegający na sprawdzeniu, czy dany katalog jest widoczny w systemie plików. Programiści robią to, by uniknąć błędów podczas próby manipulacji nieistniejącymi katalogami.

## Jak to zrobić:

Podstawowy kod w Fish Shell-u, który pozwala sprawdzić, czy katalog istnieje, wygląda tak:

```fish
if test -d /scieżka/do/katalogu
    echo "Katalog istnieje"
else 
    echo "Katalog nie istnieje"
end
```

Więc jesli mamy katalog o nazwie "dom", output będzie taki:

```fish
~> if test -d /dom
      echo "Katalog istnieje"
  else 
      echo "Katalog nie istnieje"
  end
Katalog istnieje
```

## Pogłębiona analiza:

Sprawdzanie, czy katalog istnieje, to jedne z podstawowych operacji, które są wykonywane od początku istnienia systemów operacyjnych. Bez tej możliwości pracowanie z systemem plików byłoby niebezpieczne i nieefektywne.

Alternatywą dla powyższego kodu jest zastosowanie funkcji `test -e`, która sprawdza, czy ścieżka istnieje, niezależnie od tego, czy jest to plik, czy katalog.

Decydując się na użycie `-d` zamiast `-e`, mamy dodatkową pewność, że ścieżka prowadzi do katalogu, a nie do pliku.

## Zobacz także:

Dodatkowe informacje na temat Fish Shell oraz funkcji `test` znajdziesz w tych źródłach:

1. Dokumentacja Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
2. Wirtualny kurs Fish Shell: [http://fishshell.com/tutorial.html](http://fishshell.com/tutorial.html)
3. Opis funkcji `test`: [https://fishshell.com/docs/current/cmds/test.html](https://fishshell.com/docs/current/cmds/test.html)