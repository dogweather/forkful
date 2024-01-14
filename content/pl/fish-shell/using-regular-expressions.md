---
title:                "Fish Shell: Używanie wyrażeń regularnych"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

#Dlaczego

Korzystanie z wyrażeń regularnych jest niezbędne do szybkiego i efektywnego programowania w języku Fish Shell. Dzięki nim możemy znacznie uprościć pracę z tekstem, wykorzystując zaawansowane wzorce do wyszukiwania, zamiany i manipulacji tekstem.

##Jak to zrobić

Pierwszym krokiem jest przypisanie wzorca do zmiennej. W poniższym przykładzie, wykorzystamy wszelkie cyfry do wyszukania określonego tekstu:

```
set pattern "[0-9]+"
```

Następnie, używając funkcji `string match` mamy możliwość sprawdzenia, czy dany tekst pasuje do naszego wzorca:

```
set text "123456"
if string match -q $pattern $text
echo "Tekst pasuje do wzorca"
end
```

Wynik wyżej to "Tekst pasuje do wzorca", ponieważ wyrażenie "[0-9]+" pasuje do tekstu "123456". Przykład ten jest bardzo prosty, ale wykorzystując bardziej skomplikowane wzorce, możemy przeprowadzić bardziej zaawansowane operacje, takie jak zamiana tekstu czy filtrowanie danych.

##Głębsza analiza

Wyrażenia regularne w języku Fish Shell są oparte na wyrażeniach regularnych w języku Perl, więc wiele symboli i wzorców jest podobnych lub identycznych w obu językach. Możemy również wykorzystać operator `=~` do sprawdzania dopasowania wzorca do tekstu:

```
if begin
  set text "To jest przykładowy tekst"
  and string match -q "tekst" $text
  and string match -q "przykład" $text
end
echo "Wyrażenie pasuje"
end
```

Ten przykład pokazuje, jak możemy wykorzystywać konstrukcje warunkowe w połączeniu z wyrażeniami regularnymi do bardziej zaawansowanych zadań manipulacji tekstem.

#Zobacz też

- https://fishshell.com/docs/current/cmds/set.html
- https://fishshell.com/docs/current/cmds/string.html
- https://fishshell.com/docs/current/index.html#overview