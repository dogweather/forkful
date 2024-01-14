---
title:    "Ruby: Odczytywanie pliku tekstowego"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś przeczytać plik tekstowy w swoim programie w Ruby? Może szukałeś sposobu na odczytanie danych z pliku CSV lub pliku dziennika zdarzeń? Zarówno dla początkujących, jak i doświadczonych programistów, umiejętność czytania plików jest niezbędna, aby praca z danymi była łatwiejsza i bardziej efektywna. W tym artykule dowiesz się, jak w prosty sposób odczytywać pliki tekstowe w języku Ruby.

## Jak to zrobić

Pierwszą rzeczą, którą musisz zrobić, jest otwarcie pliku, z którym chcesz pracować. W tym celu użyj metody `File.open()` i podaj nazwę pliku oraz tryb, w jakim chcesz go otworzyć. Na przykład, jeśli chcesz przeczytać plik tekstowy `dane.txt`, użyj następującego kodu:

```Ruby
plik = File.open("dane.txt", "r")
```

Pamiętaj, że tryb "r" oznacza, że plik jest otwarty tylko do odczytu. Następnie możesz użyć metody `read()` do odczytania zawartości pliku i przypisać ją do zmiennej. Na przykład:

```Ruby
zawartosc = plik.read()
```

Teraz możesz wyświetlić zawartość pliku za pomocą metody `puts`:

```Ruby
puts zawartosc
```

Spodziewaj się zobaczyć w konsoli zawartość pliku `dane.txt`. Innym sposobem odczytania pliku jest użycie pętli `each_line`, która pozwala na przetworzenie każdej linii pliku osobno. Na przykład:

```Ruby
plik.each_line do |linia|
  puts linia
end
```

To pozwoli na wyświetlenie każdej linii pliku oddzielnie.

## Głębsza analiza

W języku Ruby istnieje również możliwość przekazywania blokowego do metody `File.open()`, co jest przydatne przy pracach z plikami. Oznacza to, że możesz wywołać metodę `open()` z blokiem kodu, który zostanie wykonany wewnątrz tej metody. Na przykład, jeśli chcesz wykonać jakieś działanie na każdej linii pliku, możesz to zrobić w ten sposób:

```Ruby
File.open("dane.txt", "r") do |plik|
  plik.each_line do |linia|
    # wykonaj działanie na linii
  end
end
```

Podczas pisania kodu odczytującego pliki, ważne jest również pamiętanie o zamknięciu pliku po zakończeniu operacji na nim. W naszym przykładzie, kiedy używaliśmy metody `read()`, musimy również użyć metody `close()` aby zwolnić plik i zapobiec utracie zasobów systemowych. Użycie bloku kodu z metodą `File.open()` automatycznie wywoła metodę `close()` przy zakończeniu działania.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej na temat odczytywania i pisania plików tekstowych w języku Ruby, możesz zapoznać się z oficjalną dokumentacją Ruby oraz z poniższymi artykułami:

- [Ruby: Dokumentacja - Class: File](https://ruby-doc.org/core-2.7.2/File.html)
- [Odczytywanie i zapisywanie plików w języku Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Ruby - Odczytywanie plików tekstowych w praktyce](https://michal