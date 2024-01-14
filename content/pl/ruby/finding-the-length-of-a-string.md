---
title:    "Ruby: Znajdowanie długości ciągu znaków"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często, w trakcie programowania w Ruby, potrzebujemy znaleźć długość łańcucha. Może to być wykorzystane do liczenia znaków w ciągach tekstowych, sprawdzania poprawności wprowadzonego hasła lub po prostu do wyświetlenia informacji użytkownikowi. W tym krótkim artykule omówię, jak znaleźć długość łańcucha za pomocą prostych kodów w języku Ruby.

## Jak to zrobić

Aby znaleźć długość łańcucha w Ruby, użyjemy metody `.length`. Najpierw musimy przypisać łańcuch do zmiennej, na przykład:

```Ruby
text = "Witaj, świecie!"
```

Następnie, możemy wywołać metodę `.length` na zmiennej `text`:

```Ruby
puts text.length
```

Powyższy kod wyświetli wartość `15`, ponieważ łańcuch składa się z 15 znaków (w tym spacji). Możemy również użyć tej metody bezpośrednio na łańcuchu, bez przypisywania go do zmiennej:

```Ruby
puts "Hello, World!".length
```

Powyższy kod również wyświetli wartość `15`.

Możemy również wykorzystać metodę `.length` do sprawdzania długości wprowadzanego przez użytkownika hasła. Przykładowo, jeśli chcemy, aby hasło miało co najmniej 8 znaków, możemy użyć warunku:

```Ruby
print "Wprowadź hasło: "
password = gets.chomp
if password.length >= 8
  puts "Hasło jest wystarczająco długie."
else
  puts "Hasło jest za krótkie."
end
```

Ten przykład pokazuje, jak prosto, ale przydatnie, możemy wykorzystać metodę `.length` w swoim kodzie Ruby.

## Deep Dive

Długość łańcucha jest liczbą znaków znajdujących się w łańcuchu, włączając w to spacje i znaki specjalne. Dlatego, metoda `.length` działa w prosty sposób - po prostu zlicza wszystkie znaki w łańcuchu.

Warto również wspomnieć, że metoda `.length` może być używana na innych typach danych, takich jak tablice czy hashe. W przypadku tablic, metoda ta zwróci liczbę elementów w tablicy, a w przypadku haszy - liczbę par klucz-wartość.

## Zobacz także

- Oficjalna dokumentacja Ruby o metodzie `.length`: https://ruby-doc.org/core-2.7.2/String.html#method-i-length
- Przykładowe zadania z wykorzystaniem metody `.length`: https://www.codewars.com/kata/search/ruby?q=length