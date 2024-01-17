---
title:                "Att använda reguljära uttryck"
html_title:           "Elixir: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

Vad & Varför?
Regular expressions används för att söka efter mönster i textsträngar och är ett kraftfullt verktyg för programmerare. Detta hjälper till att hantera och manipulera data på ett mer effektivt sätt.

Hur man:
För att använda regular expressions i Elixir, behöver du använda Regex-modulen. Syntaxen är liknande som i andra språk, men med viss skillnad. Här är ett exempel på hur man söker efter en email-adress i en textsträng:
Elixir Regex.scan(~r/[\w\d]+@[\w\d]+\.[\w\d]+/, "john@gmail.com")
=> [match: "john@gmail.com"]

Djupdykning:
Regular expressions har funnits i över 60 år och har genomgått många förbättringar under åren. Det finns också alternativ till Regex-modulen, som till exempel PCRE (Perl Compatible Regular Expressions). Implementationen av regular expressions i Elixir är optimerad för att vara snabb och effektiv.

Se även:
- https://hexdocs.pm/elixir/Regex.html
- https://www.erlang.org/doc/man/regexp.html