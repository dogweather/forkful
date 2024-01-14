---
title:                "Ruby: Tworzenie pliku tymczasowego"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Dlaczego

Tworzenie plików tymczasowych jest bardzo przydatnym narzędziem w Ruby. Służą one do przechowywania danych tymczasowo, co jest szczególnie przydatne podczas przetwarzania dużych ilości danych lub operowania na plikach. W tym artykule dowiesz się, dlaczego tworzenie tymczasowych plików jest ważne i jak to zrobić w najprostszy sposób.

## Jak

Tworzenie plików tymczasowych w Ruby jest bardzo proste. Wystarczy użyć wbudowanej metody `Tempfile.new`, która automatycznie tworzy i otwiera plik tymczasowy. Poniżej znajdują się przykładowe kody oraz wynik wyjściowy.

```Ruby
require 'tempfile'

# Tworzenie pliku tymczasowego
tempfile = Tempfile.new('dane')

# Wyświetlanie ścieżki do pliku tymczasowego
puts tempfile.path
```
Output:
`/var/folders/g3/gz_2b5jn6fx0h6gkz6pq7dp00000gn/T/dane20181113-33297-190o86c`

```Ruby
# Zapisywanie danych do pliku tymczasowego
tempfile.write("To jest przykładowy tekst.")

# Odczytywanie danych z pliku tymczasowego
puts tempfile.read
```
Output:
`To jest przykładowy tekst.`

```Ruby
# Zamykanie i usuwanie pliku tymczasowego
tempfile.close
tempfile.unlink
```

W ten sposób możesz w prosty sposób tworzyć, zapisywać i odczytywać dane z pliku tymczasowego w swoim kodzie Ruby.

## Deep Dive

Warto zauważyć, że plik tymczasowy jest automatycznie usuwany po wywołaniu metody `unlink`. Dzieje się tak dlatego, że plik tymczasowy jest uważany za obiekt "zabezpieczony", czyli taki, który może zostać usunięty tylko przez swojego właściciela. Jako że w tym przypadku właścicielem jest obiekt `tempfile`, to tylko on może usunąć plik tymczasowy. Dzięki temu nie musimy się martwić, czy przypadkiem nie pozostanie on na naszym systemie.

Warto również zauważyć, że pliki tymczasowe są domyślnie usuwane po zamknięciu. Jeśli chcesz uniknąć przypadkowego usunięcia pliku, możesz przekazać dodatkowy argument `false` do metody `unlink`, w ten sposób:

```Ruby
tempfile.unlink(false)
```

W takim przypadku plik tymczasowy nie zostanie usunięty po zamknięciu, a my będziemy musieli usunąć go samodzielnie.

## Zobacz również

* [Oficjalna dokumentacja dla `Tempfile` w Ruby](https://ruby-doc.org/stdlib-2.5.1/libdoc/tempfile/rdoc/Tempfile.html)
* [Artykuł na temat tworzenia plików tymczasowych w Ruby](https://www.codecademy.com/articles/temporary-files-ruby)
* [Przykładowy kod tworzenia pliku tymczasowego w Ruby na GitHubie](https://github.com/koic/ruby-gmail/blob/master/tmp/temporary_io.rb)