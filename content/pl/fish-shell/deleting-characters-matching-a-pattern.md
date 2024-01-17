---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Fish Shell: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Usuwanie dopasowujących się znaków to popularna czynność w świecie programowania. Polega ona na usuwaniu znaków w tekście, które pasują do określonego wzorca. Programiści często wykonują tę czynność, aby uporządkować i przetworzyć duże ilości danych, lub aby dostosować tekst do swoich potrzeb.

## Jak to zrobić:

Fish Shell oferuje prosty i skuteczny sposób na usuwanie znaków odpowiadających wzorcowi. Wystarczy użyć polecenia "string delete" i podać wzorzec, który chcemy usunąć. Przykład:

```Fish Shell
string delete "l" hello
```

Output: heo 

Możemy także użyć flagi "-r" aby usunąć wszystkie wystąpienia danego znaku w tekście. Przykład:

```Fish Shell
string delete -r "a" banana
```

Output: bnn 

## Głębsze Wprowadzenie:

Usuwanie dopasowujących się znaków jest popularnym zagadnieniem w programowaniu od dawna. Początkowo wykorzystywano do tego mało wydajne rozwiązania, takie jak skrypty w języku AWK czy Perl. Obecnie, dzięki nowoczesnym narzędziom, takim jak Fish Shell, możemy wykonywać tę czynność szybko i efektywnie. Alternatywą dla polecenia "string delete" w Fish Shell może być również użycie narzędzi takich jak sed lub tr.

## Zobacz również:

Jeśli chcesz dowiedzieć się więcej o poleceniu "string delete" i innych przydatnych komendach, polecam zajrzeć do oficjalnej dokumentacji Fish Shell: https://fishshell.com/docs/current/commands.html.