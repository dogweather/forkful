---
title:    "Ruby: Tworzenie tymczasowego pliku"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Dlaczego tworzymy pliki tymczasowe?

Tworzenie plików tymczasowych jest powszechnie stosowaną praktyką w programowaniu. Są one szczególnie przydatne w przypadkach, gdy program musi tymczasowo przechowywać dane lub informacje, które nie są potrzebne w trwałej formie. Tworzenie plików tymczasowych pozwala również na zachowanie porządku w systemie plików, ponieważ zostaną one automatycznie usunięte po zakończeniu ich wykorzystywania.

## Jak utworzyć plik tymczasowy w Ruby?

Aby utworzyć plik tymczasowy w Ruby, używamy metody `tempfile` dostępnej w module `File`. Poniżej przedstawiony jest przykład kodu, który tworzy plik tymczasowy i zapisuje w nim tekst "To jest przykładowy tekst!".

```Ruby
require 'tempfile'

#tworzenie pliku tymczasowego
tymczasowy_plik = Tempfile.new('plik')

#zapisywanie danych do pliku
tymczasowy_plik.puts "To jest przykładowy tekst!"

#wyświetlenie zawartości pliku
puts tymczasowy_plik.read

#zamknięcie i usunięcie pliku
tymczasowy_plik.close
tymczasowy_plik.unlink
```

W powyższym przykładzie wykorzystaliśmy metody `puts` i `read` do zapisania i odczytania danych z pliku tymczasowego. Aby zamknąć i usunąć plik, musimy wywołać odpowiednio metody `close` i `unlink`.

## Głębszy wgląd w tworzenie pliku tymczasowego

Plik tymczasowy jest tworzony w systemie plików, jednak jest on niewidoczny dla użytkownika. Zostanie on usunięty automatycznie po zamknięciu i usunięciu lub po zakończeniu działania programu. W przypadku, gdy plik taki nie zostanie usunięty, zostanie on przeniesiony do katalogu tymczasowego systemu operacyjnego.

Istnieje również możliwość ustawienia określonej ścieżki, w której zostanie utworzony plik tymczasowy. W tym celu możemy przekazać ścieżkę w drugim argumencie metody `tempfile`, na przykład `Tempfile.new('plik', '/uzytkownik/home/')`. Ta funkcjonalność pozwala na większą kontrolę nad tworzeniem plików tymczasowych.

# Zobacz również

- [Dokumentacja Ruby o tworzeniu plików tymczasowych](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [Poradnik z przykładami dotyczącymi tworzenia plików tymczasowych w Ruby](https://www.rubyguides.com/2018/07/ruby-tempfile/)