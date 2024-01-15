---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Ruby: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiałeś się kiedyś, dlaczego ktoś chciałby usunąć znaki pasujące do wzorca? Może jest to potrzebne do filtrowania tekstu lub po prostu aby uporządkować dane. W tym artykule dowiesz się, jak w prosty sposób usunąć znaki pasujące do wzorca w języku Ruby.

## Jak to zrobić?

``` Ruby
str = "Hello World!"
p str.delete "l" 
=> Heo Word!
```

W powyższym przykładzie użyto metody `delete`, która usuwa wszystkie wystąpienia określonego znaku lub znaków podanego jako argument. Można również podać więcej niż jeden znak do usunięcia, na przykład `str.delete "lLoO"`. 

Możesz również użyć wyrażenia regularnego jako argumentu metody `delete`, jeśli chcesz usunąć więcej niż jeden konkretne znak. Na przykład:

``` Ruby
str = "Hello World!"
p str.delete /[aeiou]/
=> Hll Wrld!
```

W powyższym przykładzie użyto wyrażenia regularnego `[aeiou]` w celu usunięcia wszystkich samogłosek z ciągu znaków `str`.

## Deep Dive

Usunięcie znaków pasujących do określonego wzorca może być również bardzo przydatne, gdy chcesz oczyścić dane. Na przykład, jeśli masz duży plik z danymi, możesz użyć metody `delete` w połączeniu z instrukcją `gsub` w celu usunięcia zbędnych znaków lub wyrażeń.

``` Ruby
File.open("data.txt", "r") do |file|
  file.each_line do |line|
    # usuń wszystkie spacje i przecinki z tekstu
    clean_line = line.gsub(/[ ,]/, "")
    # wyświetl wynik
    p clean_line
  end
end
```

W tym przykładzie, każda linia w pliku `data.txt` jest oczyszczana z występujących w niej spacji i przecinków, a następnie wyświetlana na ekranie.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o operacjach na ciągach znaków w języku Ruby, sprawdź poniższe źródła:

- [Dokumentacja Ruby - Metoda `delete`](https://ruby-doc.org/core-2.7.2/String.html#method-i-delete)
- [Tutorial Ruby - Operacje na ciągach znaków](https://www.rubyguides.com/ruby-string/)
- [Dokumentacja Ruby - Wyrażenia regularne](https://ruby-doc.org/core-2.7.2/Regexp.html)