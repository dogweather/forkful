---
title:                "Ruby: Odczytywanie pliku tekstowego"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego ludzie w ogóle czytają pliki tekstowe w swoim kodzie Ruby? W tym krótkim artykule przekażemy Ci ważne informacje na ten temat i pokażemy, jak możesz w łatwy sposób czytać pliki tekstowe w swoim własnym kodzie.

## Jak To Zrobić

Czytanie plików tekstowych w Ruby jest bardzo proste. Wystarczy skorzystać z wbudowanej metody `File.open` i podać jej nazwę pliku jako argument, aby otworzyć plik do odczytu. Następnie można użyć metody `readlines`, aby odczytać zawartość pliku linia po linii.

```Ruby
plik = File.open("nazwa_pliku.txt", "r")
puts plik.readlines
plik.close
```

Powyższy kod otworzy plik o nazwie "nazwa_pliku.txt" i wyświetli jego zawartość na konsoli. Należy pamiętać, aby zawsze zamknąć plik po zakończeniu jego odczytu za pomocą metody `close`, aby uniknąć problemów z pamięcią.

## Pogłębione Podejście

Ponadto, w Ruby istnieje wiele innych metod, które mogą zostać wykorzystane do odczytywania i przetwarzania plików tekstowych. Na przykład, można użyć metody `read` do odczytania całej zawartości pliku w jednym ciągu, lub metody `gets` do odczytywania jednej linii na raz.

```Ruby
plik = File.open("nazwa_pliku.txt", "r")
puts plik.read
plik.close
```

```Ruby
plik = File.open("nazwa_pliku.txt", "r")
puts plik.gets
plik.close
```

Oprócz tego, w Ruby można także wykorzystać wyrażenia regularne do wyszukiwania i manipulowania tekstem odczytanym z pliku.

## Zobacz również
- [Dokumentacja Ruby - Odczytywanie plików](https://ruby-doc.org/core-2.6/File.html#method-c-open)
- [Tutorial Ruby - Czytanie i zapisywanie plików](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Forum Ruby - Dyskusja na temat odczytywania plików](https://www.ruby-forum.com/t/reading-files/219147)