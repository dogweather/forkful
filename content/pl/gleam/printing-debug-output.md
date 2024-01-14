---
title:    "Gleam: Wyświetlanie wyników debugowania"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Wykorzystywanie drukowania danych debugowania jest niezwykle przydatne w programowaniu, ponieważ pomaga zrozumieć działanie kodu i rozwiązać ewentualne problemy. Jest to też ważna część procesu debugowania i doskonały sposób na zwiększenie efektywności swojej pracy.

## Jak to zrobić

Drukowanie danych debugowania jest bardzo proste w języku programowania Gleam. Wystarczy użyć metody `io.inspect` i podać jako argument dowolną wartość, którą chcemy wyświetlić. Możemy także wykorzystać specjalną składnię do wypisania wartości w określonym formacie. Poniżej znajdują się przykłady kodu oraz odpowiadające im wyjścia.

```Gleam
let name = "John"
let age = 30
io.inspect("Witaj", name, "masz", age, "lat")
```
Wyjście:
```
Witaj John, masz 30 lat
```
```Gleam
let players = ["John", "Kate", "Mike"]
#"$ = duże litery, ; = kończący znak
io.inspect("Lista graczy:", players, format="$", delimiter=";")
```
Wyjście:
```
Lista graczy: JOHN; KATE; MIKE;
```

## Głębszy zanurzenie

Istnieje wiele możliwości wykorzystania drukowania danych debugowania w języku Gleam, takich jak wyświetlanie wartości zmiennych w różnych kontekstach czy też kontrolowane wypisywanie informacji podczas wykonywania testów. Dodatkowo, istnieje możliwość zastosowania funkcji `io.inspect` do dowolnego typu danych, dzięki czemu możemy dokładnie przeanalizować ich strukturę i znaleźć potencjalne błędy w naszym kodzie.

## Zobacz też

- Dokumentacja języka Gleam: https://gleam.run/
- Przewodnik po debugowaniu w Gleam: https://gleam.run/book/tour/debugging.html
- Przykładowe projekty w języku Gleam: https://github.com/search?q=language%3Agleam