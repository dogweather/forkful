---
title:                "Ruby: Znalezienie długości ciągu znaków"
simple_title:         "Znalezienie długości ciągu znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie kodu jest jak rozwiązywanie łamigłówek - zawsze istnieje więcej niż jedno możliwe rozwiązanie. Często musimy wykonać pewne operacje na tekstach i jedną z takich czynności jest obliczenie długości tekstu. W tym artykule dowiesz się jak w prosty sposób znaleźć długość ciągu znaków w języku Ruby.

## Jak To Zrobić

Aby znaleźć długość ciągu znaków możemy skorzystać z metody `length` lub `size`, które zwracają liczbę znaków w danym tekście. Poniżej znajduje się przykładowy kod wraz z odpowiadającym mu wynikiem.

```Ruby
text = "Cześć, jestem blogiem o programowaniu!"
puts text.length # Output: 35
puts text.size # Output: 35
```

W powyższym przykładzie została utworzona zmienna `text`, do której przypisany został ciąg znaków. Następnie wywołujemy metodę `length` oraz `size` i wyświetlamy jej wynik za pomocą funkcji `puts`. Oba sposoby zwracają tę samą wartość, więc możesz użyć dowolnej z nich.

Jeśli chcesz uzyskać długość tekstu zawierającego polskie znaki, takie jak `ł` czy `ż`, możesz skorzystać z metody `bytesize`, która zwraca ilość bajtów w tekście. Przykład:

```Ruby
text = "Światło w międzynarodowym języku programowania"
puts text.bytesize # Output: 49
```

Warto również zauważyć, że metoda `length`, `size` oraz `bytesize` mogą być używane również do obliczania długości tablicy lub hasha.

## Deep Dive

Podczas tworzenia aplikacji internetowych często potrzebujemy sprawdzić, czy użytkownik podał odpowiednią długość hasła lub imienia. Wtedy bardzo przydatną funkcją jest metoda `validates_length_of`, która pozwala nam określić minimalną i maksymalną długość ciągu znaków. Przykład:

```Ruby
class User < ApplicationRecord
  validates_length_of :username, in: 5..20 # username musi mieć od 5 do 20 znaków
  validates_length_of :password, minimum: 8 # hasło musi mieć minimum 8 znaków
end
```

Dzięki użyciu tej metody możemy zapewnić, że użytkownicy podadzą odpowiednio długie informacje, co pozwoli nam zwiększyć bezpieczeństwo naszej aplikacji.

## Zobacz również

* [Dokumentacja Ruby o metodzie `length`](https://ruby-doc.org/core-2.6/String.html#method-i-length)
* [Dokumentacja Ruby on Rails o walidacji długości modeli](https://guides.rubyonrails.org/active_record_validations.html#length)
* [Kurs Ruby na Codecademy](https://www.codecademy.com/learn/learn-ruby)