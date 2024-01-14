---
title:                "Gleam: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en viktig del av programmering eftersom det gör det möjligt att snabbt och enkelt ändra text i stora mängder av kod. Det är också ett bra sätt att göra upprepande uppgifter mer effektiva.

## Hur man gör det

För att söka och ersätta text i Gleam, använd ```replace``` funktionen. Till exempel, om du vill ändra alla förekomster av ordet "hund" till "katt" i en sträng, skulle du skriva:

```Gleam
let str = "Jag har en hund som heter Fido"
let ny_sträng = replace(str, "hund", "katt")
```

Detta kommer att resultera i strängen "Jag har en katt som heter Fido". Du kan också använda regex (regular expressions) för att söka efter mönster istället för specifika ord. Till exempel:

```Gleam
let str = "Jag älskar att programmera på Gleam"
let ny_sträng = replace(str, regex"#Gleam", "Elixir")
```

Detta kommer att ersätta alla förekomster av "Gleam" med "Elixir" och resultera i strängen "Jag älskar att programmera på Elixir".

## Djupdykning

I Gleam kan du också använda ```replace_nth``` funktionen för att ersätta den n:te förekomsten av ett mönster i en textsträng. Detta kan vara användbart om du bara vill ersätta vissa förekomster istället för alla. Till exempel:

```Gleam
let str = "Jag har en hund som heter Fido. Jag har en katt som heter Whiskers."
let ny_sträng = replace_nth(str, "har", "äger", 2)
```

Detta kommer att ersätta det andra förekomsten av "har" med "äger" och resultatet blir "Jag har en hund som heter Fido. Jag äger en katt som heter Whiskers". Du kan också använda ```replace_all``` för att ersätta alla förekomster utan att behöva ange ett specifikt nummer.

## Se även

- [Officiell Gleam dokumentation för replace och replace_all](https://gleam.run/)
- [Regex tutorial för Gleam](https://medium.com/@kay.jones/regular-expressions-in-gleam-a1133