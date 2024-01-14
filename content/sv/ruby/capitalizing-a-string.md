---
title:                "Ruby: Att göra en sträng stor"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Det är väldigt vanligt att man behöver konvertera en sträng så att första bokstaven i varje ord är stor. Det kan vara för att presentera information eller bara för estetiskt syfte. I denna blogginlägg kommer vi att lära oss hur man gör det med Ruby-programmeringsspråket.


## Hur man gör

För att börja, måste vi först definiera en sträng som vi vill ändra. Låt oss använda "hej världen" som ett exempel.

```Ruby
sträng = "hej världen"
```
Nu, för att konvertera strängen med stor bokstav på varje ord, kan vi använda metoden "capitalize".

```Ruby
sträng.capitalize
```

Detta kommer att ge oss följande utmatning:

```Ruby
"Hej världen"
```

Om du vill ändra endast den första bokstaven i hela strängen kan du använda metoden "capitalize!".

```Ruby
sträng.capitalize!
```
Nu kommer strängen att bli permanent ändrad till:

```Ruby
"Hej världen"
```

## Djupdykning

Om vi gräver lite djupare i hur metoden "capitalize" fungerar, kommer vi att märka att det i själva verket konverterar endast den första bokstaven i varje ord, medan resten av bokstäverna förblir oförändrade. Om strängen innehåller en accent eller specialtecken, kommer de fortfarande att behållas efter konverteringen.

Det finns också en annan metod som heter "titleize" som fungerar på liknande sätt som "capitalize", förutom att den också konverterar alla mellanrum till "-" tecken. Detta kan vara användbart om du till exempel vill skapa en URL från en sträng.

## Se även

Här är några användbara länkar för att lära dig mer om strängmanipulation i Ruby:

- [Ruby dokumentation för metoden "capitalize"](https://ruby-doc.org/core-2.6.1/String.html#method-i-capitalize)
- [En handledning om strängmanipulation i Ruby](https://www.digitalocean.com/community/tutorials/how-to-use-string-manipulation-methods-in-ruby)
- [En