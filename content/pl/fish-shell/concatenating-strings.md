---
title:                "Łączenie ciągów znaków"
html_title:           "Fish Shell: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli często pracujesz z zasobami tekstowymi lub plikami, na pewno spotkałeś się z koniecznością łączenia (konkatenacji) ciągów znaków. Jest to często wykorzystywana operacja w programowaniu, pozwalająca na tworzenie nowych informacji na podstawie już istniejących. W tym artykule dowiesz się, jak używać narzędzia Fish Shell do konkatenacji stringów w prosty i skuteczny sposób.

## Jak to zrobić?

Fish Shell posiada wbudowane funkcje, które umożliwiają łatwe łączenie stringów. Aby to zrobić, wystarczy użyć operatora "++", który dodaje dwa ciągi znaków razem, tworząc nowy ciąg. Na przykład:

```Fish Shell
set first_name "Jan"
set last_name "Kowalski"
set full_name $first_name" "$last_name
echo $full_name
```

W tym przykładzie, najpierw tworzymy zmienne "first_name" i "last_name" przechowujące imię i nazwisko. Następnie używamy operatora "++" do połączenia tych dwóch zmiennych i przypisujemy wynik do zmiennej "full_name". W końcu, korzystając z komendy "echo", wyświetlamy zawartość zmiennej "full_name", która jest połączeniem obu ciągów.

## Deep Dive

Fish Shell oferuje również funkcję "string join", która pozwala na łączenie listy stringów w jeden ciąg znaków. Na przykład:

```Fish Shell
set fruits "jabłko" "banan" "truskawka"
string join " " $fruits
```

W tym przykładzie, tworzymy zmienną "fruits", która przechowuje listę trzech różnych owoców. Następnie używamy funkcji "string join", która pozwala na połączenie wszystkich elementów listy w jedno wyrażenie, z separatorem (spacją w tym przypadku) pomiędzy poszczególnymi elementami. 

## Zobacz również

* Oficjalna dokumentacja Fish Shell: https://fishshell.com/docs/current/index.html
* Samouczek Fish Shell: https://fishshell.com/docs/current/tutorial.html
* Popularne komendy Fish Shell: https://fishshell.com/docs/current/cmds.html