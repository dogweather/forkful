---
title:                "Ruby: Weryfikacja istnienia katalogu"
simple_title:         "Weryfikacja istnienia katalogu"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Ruby, z pewnością już wiele razy spotkałeś się z koniecznością sprawdzenia, czy dany katalog istnieje. To ważna umiejętność, która pomaga utrzymać porządek w naszych aplikacjach i uniknąć błędów. W tym artykule przyjrzymy się temu, dlaczego warto nauczyć się sprawdzać istnienie katalogów w Ruby.

## Jak to zrobić

Sprawdzenie istnienia katalogu w Ruby jest proste - wystarczy wykorzystać metodę `Dir.exist?()` i przekazać jej jako argument ścieżkę do sprawdzenia. Poniżej przedstawiamy kilka przykładów, jak wykorzystać tę metodę w praktyce:

```Ruby
# Sprawdzenie istnienia katalogu "projekty"
puts Dir.exist?("projekty") # output: true

# Przekazanie ścieżki jako zmienną
sciezka = "dokumenty/raporty"
puts Dir.exist?(sciezka) # output: true

# Zastosowanie warunku if - else
if Dir.exist?("zdjecia")
  puts "Katalog istnieje"
else
  puts "Katalog nie istnieje"
end
```

Jak widać, wykorzystanie metody `Dir.exist?()` jest bardzo proste i pozwala szybko sprawdzić istnienie dowolnego katalogu w naszym systemie plików.

##Głębsza analiza

Chcesz dowiedzieć się więcej o sposobie działania metody `Dir.exist?()`? Podpowiadamy, jak to działa pod spodem. Ta metoda wykorzystuje API systemowe do sprawdzenia, czy dana ścieżka istnieje i jest katalogiem. W przypadku systemu Unix jest to funkcja `stat()` z biblioteki `unistd.h`, a w systemie Windows wykorzystywana jest funkcja `GetFileAttributes()` z biblioteki `windows.h`. W obu przypadkach metoda zwraca wartość logiczną `true` lub `false` na podstawie wyniku działania funkcji systemowej.

## Zobacz także

Jeśli chcesz poszerzyć swoją wiedzę na temat pracy z plikami i katalogami w Ruby, polecamy zapoznać się z poniższymi linkami:

- [Dokumentacja Ruby o sprawdzaniu istnienia plików i katalogów](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F)
- [Artykuł na temat wykorzystania API systemowego w Ruby](https://praveenperera.com/how-ruby-works-under-the-hood-part7/)
- [Poradnik na temat pracy z plikami i katalogami w Ruby](https://www.rubyguides.com/2015/04/working-with-directories-in-ruby/)

Dziękujemy za lekturę! Mamy nadzieję, że ten artykuł przydał Ci się w pracy z Ruby. Do zobaczenia w kolejnych wpisach!