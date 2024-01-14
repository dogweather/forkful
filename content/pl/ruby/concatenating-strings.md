---
title:    "Ruby: Łączenie ciągów znaków."
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak połączyć różne ciągi znaków w jedno? A może potrzebujesz stworzyć dużą wiadomość z kilku sekcji? W takich przypadkach bardzo przydatna okazuje się funkcja "concat" w języku Ruby. Pozwala ona na łączenie różnych ciągów znaków w jeden, co jest niezwykle przydatne w wielu sytuacjach. Dlatego też postanowiliśmy przygotować dla Ciebie krótki przewodnik, który pomoże Ci zrozumieć, jak działa ta funkcja.

## Jak to zrobić

Aby połączyć różne ciągi znaków w jedno, należy użyć funkcji "concat" i podać jako argumenty ciągi, które chcemy połączyć. Poniżej znajduje się przykładowy kod w języku Ruby:

```Ruby 
message_part_1 = "Witaj"
message_part_2 = "na"
message_part_3 = "naszym"
message_part_4 = "blogu!"

concat_message = message_part_1.concat(message_part_2, message_part_3, message_part_4)

puts concat_message 
```
Output:
"Witaj na naszym blogu!"

Jak widać, funkcja "concat" łączy wszystkie ciągi znaków w jeden i zwraca go jako wynik. Jest to bardzo przydatne, gdy chcemy dodać kilka elementów do jednej wiadomości lub stringa.

## Głębszy zanurzenie

Warto również wspomnieć, że funkcja "concat" jest tylko jednym z wielu sposobów na łączenie ciągów znaków w języku Ruby. Można także użyć operatora "+", na przykład:

```Ruby 
message_part_1 = "Witaj"
message_part_2 = "na"
message_part_3 = "naszym"
message_part_4 = "blogu!"

concat_message = message_part_1 + " " + message_part_2 + " " + message_part_3 + " " + message_part_4 

puts concat_message 
```
Output:
"Witaj na naszym blogu!"

Innym sposobem jest użycie metody "join", która pozwala na łączenie elementów tablicy w jeden string. Przykład:

```Ruby 
message_parts = ["Witaj", "na", "naszym", "blogu!"]

concat_message = message_parts.join(" ")

puts concat_message 
```
Output:
"Witaj na naszym blogu!"


## Zobacz również

Jeśli chcesz dowiedzieć się więcej o łączeniu ciągów znaków w języku Ruby, polecamy zapoznanie się z poniższymi źródłami:

- Dokumentacja Ruby: https://ruby-doc.org/core-2.7.1/String.html#method-i-concat
- Wideo tutorial na temat łączenia ciągów znaków w Ruby: https://www.youtube.com/watch?v=bZpE2s9EUO4