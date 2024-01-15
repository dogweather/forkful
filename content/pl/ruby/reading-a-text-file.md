---
title:                "Wczytywanie pliku tekstowego"
html_title:           "Ruby: Wczytywanie pliku tekstowego"
simple_title:         "Wczytywanie pliku tekstowego"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, prawdopodobnie często musisz pracować z plikami tekstowymi. Może to być plik konfiguracyjny, plik z danymi do analizy lub po prostu notatka z kodem. W tym artykule dowiesz się, jak czytać pliki tekstowe za pomocą języka Ruby i jak to może ułatwić Twoją pracę.

## Jak to zrobić

Aby odczytać plik tekstowy w języku Ruby, pierwszą rzeczą, którą musisz zrobić, to otworzyć plik. Możesz to zrobić za pomocą metody `File.open`, która przyjmuje dwa argumenty: nazwę pliku oraz trybu dostępu. Na przykład, jeśli chcesz odczytać plik `dane.txt`, możesz to zrobić w następujący sposób:

```ruby
File.open('dane.txt', 'r') do |file|
  # kod, który będzie wykonywany na otwartym pliku
end
```

W tym przykładzie, tryb dostępu `'r'` oznacza, że plik będzie otwarty do odczytu. Aby móc odczytać zawartość pliku, musisz użyć metody `read` na obiekcie `file`:

```ruby
File.open('dane.txt', 'r') do |file|
  file.read # odczytaj zawartość pliku
end
```

Metoda `read` zwraca całą zawartość pliku jako pojedynczy string. Jeśli chcesz odczytać plik linia po linii, możesz użyć metody `readlines`:

```ruby
File.open('dane.txt', 'r') do |file|
  file.readlines.each do |line|
    # kod, który będzie wykonywany dla każdej linii pliku
  end
end
```

W tym przypadku, metoda `readlines` zwraca tablicę zawierającą wszystkie linie z pliku. Możesz użyć metody `each` do iteracji po tej tablicy i wykonać odpowiednie operacje dla każdego elementu.

## Deep Dive

Nieco głębiej, możesz użyć bloku kodu w metodzie `File.open` do dostępu do innych informacji o pliku, takich jak jego rozmiar, data modyfikacji czy uprawnienia dostępu. Możesz również ustawić odpowiedni tryb dostępu, na przykład do zapisu (`'w'`) lub edycji (`'a'`).

```ruby
File.open('dane.txt', 'r+') do |file|
  file.write('nowa linia') # zapisz nową linię do pliku
  file.rewind # przewiń do początku pliku
  file.read # odczytaj cały plik
end
```

Powyższy przykład używa trybu dostępu `'r+'`, który pozwala na odczyt i zapis do pliku. Metoda `rewind` jest wykorzystywana do przewinięcia do początku pliku, aby można było odczytać wszystkie linie razem z nowo dodaną.

## Zobacz również

- [Dokumentacja języka Ruby](https://ruby-doc.org/core-3.0.0/File.html)
- [Samouczek języka Ruby](https://www.ruby-lang.org/pl/documentation/quickstart/)