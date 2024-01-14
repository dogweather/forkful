---
title:    "Ruby: Generowanie losowych liczb"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest nieodłączną częścią wielu programów i aplikacji, dzięki którym możemy tworzyć różnorodne symulacje, gry czy testy. W języku Ruby istnieje wiele sposobów na generowanie przypadkowych wartości, a w tym artykule dowiecie się jak wykorzystywać je w swoich projektach.

## Jak To Zrobić

Sposób generowania liczb losowych w Ruby jest stosunkowo prosty i polega na wykorzystaniu wbudowanych w język funkcji rand oraz srand. Pierwsza z nich jest odpowiedzialna za wygenerowanie pojedynczej losowej liczby, natomiast druga pozwala na ustawienie ziarna, czyli wartości, która będzie używana do generowania liczb.

```Ruby
# Przykładowe użycie funkcji rand
puts rand # Wyświetla pojedynczą losową liczbę

# Użycie funkcji srand do ustawienia ziarna
srand(123) # Ustawia ziarno na wartość 123
puts rand # Teraz wynik będzie zawsze taki sam dla danego ziarna
```

Możemy również wykorzystać funkcję rand w celu wygenerowania losowego indeksu dla elementów z tablicy lub hasza.

```Ruby
# Generowanie losowego indeksu dla elementów w tablicy
array = ["jabłko", "banan", "truskawka", "winogrono"]
puts array[rand(4)] # Wyświetli losowy owoc z listy

# Generowanie losowego elementu z hasza
hash = {a: 1, b: 2, c: 3, d: 4}
random_key = hash.keys[rand(4)] # Wybiera losowy klucz z hasza
puts hash[random_key] # Wyświetli wartość dla wybranego losowo klucza
```

## Deep Dive

W języku Ruby mamy również dostęp do różnych metod, które pozwalają na bardziej zaawansowane generowanie liczb losowych. Warto zapoznać się z dokumentacją na ten temat, aby poznać wszystkie możliwości. Niektóre z tych metod to m.in. Random.new, Random.rand, SecureRandom czy Faker.

Możemy również ustawić ograniczenia dla generowanych liczb, jak na przykład zakres, liczby całkowite lub ułamkowe, czy typ danych (np. integer lub float).

```Ruby
# Ustawienie zakresu dla generowanych liczb
puts rand(10..20) # Wyświetli losową liczbę z zakresu od 10 do 20

# Generowanie losowej liczby całkowitej
puts rand(100) # Wyświetli losową liczbę z zakresu 0-99

# Generowanie losowej liczby ułamkowej
puts rand(0.0..1.0) # Wyświetli losową liczbę z zakresu od 0.0 do 1.0

# Generowanie losowego typu danych
puts Faker::Name.name # Wyświetli losowe imię i nazwisko
```

W języku Ruby możemy także tworzyć własne metody, które będą generować odpowiednie wartości losowe dla naszych potrzeb. Poniżej przykład funkcji, która zwraca losową liczbę z zakresu podanego jako parametry.

```Ruby
def random_number(min, max)
  rand(min..max)
end

puts random_number(5, 10) # Wyświetli losową liczbę z zakresu 5-10
```

## Zobacz Również

- Dokumentacja języka Ruby na temat generowania liczb losowych: https://ruby-doc.org/core/Kernel.html#method-i-rand
- Dokumentacja dla biblioteki Faker, umożliwiającej generowanie losowych wartości: https://github.com/faker-ruby/faker
- Przykładowy projekt wykorzystujący generowanie liczb losowych w Ruby: https://github.com/danapadila/guessing