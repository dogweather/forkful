---
title:                "Ruby: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego tak ważne jest umiejętne czytanie plików tekstowych w języku Ruby? Właśnie dlatego, że jest to kluczowa umiejętność dla każdego programisty Ruby. Bez odczytywania i przetwarzania danych z plików, tworzenie skryptów i aplikacji może stać się trudne, a nawet niemożliwe. Dlatego w tym artykule opowiemy o tym, jak wykonać tę czynność w prosty i bezproblemowy sposób.

## Jak to zrobić

Otwórzmy plik tekstowy w języku Ruby i odczytajmy jego zawartość. W tym celu wykonaj następujące kroki:

1. Użyj metody `open()` i podaj nazwę pliku, który chcesz otworzyć.
2. Następnie możesz skorzystać z metody `read()` do odczytania zawartości pliku.
3. Jeśli chcesz odczytać plik wiersz po wierszu, możesz użyć metody `each_line()`.

Przykładowy kod wyglądałby następująco:

```Ruby
file = open("plik.txt")
puts file.read() # wyświetli zawartość pliku
file.each_line { |line| puts line } # wyświetli zawartość wiersz po wierszu
```

## Głębszy zanurkowanie

Istnieje wiele ciekawych metod związanych z czytaniem plików tekstowych w języku Ruby, takich jak:

- `readlines()` - odczytuje wszystkie wiersze pliku i zapisuje je w tablicy.
- `gets()` - odczytuje tylko jeden wiersz pliku na raz.
- `closed?` - sprawdza, czy plik został zamknięty.
- `exist?` - sprawdza, czy plik istnieje.

Pamiętaj, że po zakończeniu operacji na pliku, powinieneś go zamknąć przy użyciu metody `close()`.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o pracy z plikami tekstowymi w języku Ruby, polecamy zapoznać się z następującymi linkami:

- Oficjalna dokumentacja języka Ruby na temat plików (https://ruby-doc.org/core-2.7.0/File.html)
- Przykładowy tutorial z wyjaśnieniami (https://www.rubyguides.com/2015/05/reading-files-ruby/)
- Praktyczne zastosowanie w projekcie (https://www.educative.io/edpresso/how-to-read-and-write-to-files-in-ruby)

Dziękujemy za przeczytanie naszego artykułu. Mamy nadzieję, że teraz czujesz się pewniej w czytaniu plików tekstowych w języku Ruby. Powodzenia w dalszej nauce!