---
title:    "Ruby: Usuwanie znaków pasujących do wzorca"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego
Czasami w procesie tworzenia aplikacji lub skryptów w języku Ruby, możemy napotkać na sytuację, w której chcielibyśmy usunąć określone znaki z ciągu znaków. Może to być potrzebne do filtrowania danych lub wielu innych zastosowań. W tym artykule pokażemy jak w prosty sposób usunąć znaki pasujące do określonego wzorca.

## Jak To Zrobić
Aby usunąć znaki pasujące do wzorca w Ruby, możemy skorzystać z metody `gsub` lub `delete`. Poniżej przedstawiam przykładowy kod, który pokaże jak usunąć znaki `a` z podanego ciągu.

```ruby
string = "ruby to język programowania"
filtered_string = string.gsub("a", "")
puts filtered_string
# wyjście: ruby to jzyk programowni
```

Jest to prosta i szybka metoda usuwania znaków pasujących do wzorca. Możemy również zastosować to samo rozwiązanie do usuwania wielu znaków na raz.

```ruby
string = "1,2,3 are numbers"
filtered_string = string.gsub(/[0-9]/, "")
puts filtered_string
# wyjście: , are numbers
```

Powyższy przykład wykorzystuje wyrażenia regularne, aby usunąć wszystkie cyfry z podanego ciągu.

## Przyjrzyjmy Się Temu Bliżej
Metoda `gsub` może przyjmować wyrażenie regularne lub pojedynczy znak jako pierwszy argument. Możemy również wykorzystać dodatkowe opcje, takie jak ignorowanie wielkości liter czy podanie domyślnego znaku do zastąpienia. Możliwości są szerokie i warto eksperymentować z różnymi kombinacjami.

## Zobacz Również
Jeśli chcesz dowiedzieć się więcej na temat usuwania znaków pasujących do wzorca w Ruby, zobacz poniższe linki:

- Dokumentacja Ruby: https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub
- Darmowy kurs Ruby: https://www.codecademy.com/learn/learn-ruby
- Wspólnota Ruby w Polsce: https://rubyonrails.pl/