---
title:    "Elixir: Zmiana wielkości liter w ciągu znaków"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Robienie programów w Elixir może wydawać się trudne, ale jest to język przeróżnych możliwości, który dostarcza wiele narzędzi, aby ułatwić życie programistom. Jednym z tych narzędzi jest funkcja, która pozwala nam na wielkość liter w stringu. Dlaczego więc powinno nas to zainteresować? Nie zawsze jesteśmy pewni, czy użytkownik wpisze nasze dane z poprawną wielkością liter. Aby temu zapobiec, warto zapoznać się z tą funkcją i wykorzystać ją w swoich programach.

## Jak to zrobić

Wykorzystajmy funkcję `String.capitalize/1` aby uprościć nasz kod i eliminować możliwość błędów z powodu innej wielkości liter. Sprawdźmy najpierw jak działa ta funkcja w przykładowych kodach.

```Elixir
String.capitalize("elixir") #=> "Elixir"
String.capitalize("HELLO") #=> "Hello"
```

Jak widać, funkcja ta zamienia pierwszą literę stringa na dużą, a resztę na małe litery. Możemy również wykorzystać ją w bardziej skomplikowanych sytuacjach, jak na przykład w przypadku, gdy w stringu znajdują się słowa z dużymi literami.

```Elixir
String.capitalize("hello WORLD") #=> "Hello world"
```

Warto także wspomnieć o funkcji `String.capitalize/1!`, która działa podobnie do `String.capitalize/1`, ale zmienia string na miejscu, zamiast zwracać nowy string. Poniżej znajduje się przykład jej użycia.

```Elixir
string = "elixir"
String.capitalize!(string)
string #=> "Elixir"
```

## Deep Dive

Wiele języków programowania posiada wbudowane funkcje do manipulowania stringami, ale warto zwrócić uwagę, że w Elixirze funkcje te działają inaczej niż w innych językach. Na przykład, w językach takich jak Ruby czy Python, funkcje do zmiany wielkości liter działają na całym stringu, podczas gdy w Elixirze działają tylko na pierwszym znaku. Dzięki temu możemy szybko i łatwo zmienić wyświetlanie wyrazów w aplikacji bez zmiany ich w rzeczywistości. Jednak, jeśli chcemy zmienić wielkość całego stringa, możemy wykorzystać funkcję `String.upcase/1` lub `String.downcase/1`.

## Zobacz także

- [Dokumentacja funkcji String.capitalize/1](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Porównanie funkcji do manipulowania stringami w Elixirze i innych językach](https://blog.plataformatec.com.br/2016/01/string-manipulation-in-elixir-and-ruby-part-1/)
- [Tutorial Elixir dla początkujących](https://elixirschool.com/pl/)