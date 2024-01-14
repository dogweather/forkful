---
title:    "Elixir: Łączenie ciągów znaków"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, coraz więcej programistów wybiera język programowania Elixir. Jego prostota, wydajność i skalowalność sprawiają, że jest idealny do tworzenia aplikacji internetowych, mikroserwisów i systemów rozproszonych. Jednym z często używanych funkcji w Elixir jest łączenie (concatenation) stringów. W tym artykule dowiesz się, dlaczego warto używać tej funkcji i jak ją wykorzystać w swoim kodzie.

## Jak to zrobić

W języku Elixir, łączenie stringów jest bardzo proste - używamy operatora "+" do połączenia dwóch lub więcej stringów. Na przykład:

```elixir
"Hello " + "world"
```

Wynik powyższego kodu będzie "Hello world". Zauważ, że nie musimy dodawać żadnych znaków specjalnych, takich jak "+" pomiędzy stringami, aby je połączyć.

Możemy również łączyć zmienne z typem string. Przykładowo, jeśli mamy zmienną "name" przechowującą "John", możemy użyć jej w łączeniu stringów w ten sposób:

```elixir
"Hello " + name
```

Co spowoduje wygenerowanie wyniku "Hello John". W przypadku gdy zmienna jest innego typu, zostanie automatycznie przekonwertowana na string, dzięki czemu nie musimy się martwić o typy zmiennych w łączeniu.

## Deep Dive

W Elixir, łączenie stringów jest realizowane przy pomocy funkcji z wbudowanej biblioteki Kernel - "++". Funkcja ta przyjmuje dwa argumenty (stringi) i zwraca nowy string, będący wynikiem połączenia tych dwóch. Dzięki temu, możemy łączyć nie tylko dwa, ale dowolną liczbę stringów.

```elixir
"Hello " ++ "my" ++ "friend" ++ "!"
```

Taki kod spowoduje wygenerowanie wyniku "Hello my friend!". Warto również zauważyć, że operacja łączenia stringów jest łączna, co oznacza, że kolejność dodawania nie jest istotna.

## Zobacz również

- [Dokumentacja Elixir - Concatenation](https://hexdocs.pm/elixir/Kernel.html#++/2)
- [The Elixir School - String Concatenation](https://elixirschool.com/en/lessons/basics/strings/#string-concatenation)