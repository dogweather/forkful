---
title:                "Använda reguljära uttryck"
html_title:           "Gleam: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad och varför?

Regular expressions (regex) används för att matcha mönster i strängar. Programmerare använder dem för att snabbt och effektivt utföra handlingar som att söka, matcha och ersätta tecken i en text.

## Hur man gör:

Använd `Regex` modul att arbeta med regular expressions i Elixir. Här är några exempel:

```Elixir
# Söka efter en sträng
{:ok, regex} = Regex.compile("hej")
Regex.match?(regex, "hej världen")
# => true

# Ersätta sträng
{:ok, regex} = Regex.compile("världen")
Regex.replace(regex, "hej världen", "elixer")
# => "hej elixer"
```

Som du kan se, `Regex.compile/1` används för att skapa en regular expression. Med den kan du sedan söka eller ersätta text.

## Djupdykning:

Regular expressions härstammar från teoretisk datavetenskap, särskilt formell språkteori. De har varit en grundläggande del av Unix och dess textbearbetningsverktyg sedan 70-talet.

Exempelvis finns det bibliotek som `:re2` eller `:oniguruma`. Dessa bibliotek kan vara snabbare eller mer funktionella beroende på användningsfallet, men `Regex` är inbyggd och tillräckligt kraftfull för de flesta ändamål.

När det gäller implementationen använder Elixir Erlang/OTP's Regex-bibliotek, vilket är en inbäddad wrapper för PCRE (Perl Compatible Regular Expressions).

## Se också:

- Elixir officiella dokumentation om `Regex` modul: https://hexdocs.pm/elixir/Regex.html
- Erlang/OTP's `re` modul: https://erlang.org/doc/man/re.html
- Learn You Some Erlang's kapitel om regex: http://learnyousomeerlang.com/starting-out-with-regular-expressions