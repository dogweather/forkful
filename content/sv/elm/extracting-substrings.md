---
title:                "Elm: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
I denna bloggpost kommer vi att diskutera hur man extraherar substrängar i Elm. Substrängar är delar av en större sträng som kan vara användbara i olika situationer, till exempel när man vill manipulera text eller söka efter specifika ord eller fraser. Genom att använda sig av substrängar kan du skriva effektivare och mer dynamisk kod.

## Så här gör man
För att extrahera substrängar i Elm kan du använda funktionen `String.slice start end string`. Den här funktionen tar tre argument: `start` som är indexet där du vill börja extrahera, `end` som är indexet där du vill sluta extrahera och `string` som är den ursprungliga strängen. Ett exempel på hur man kan använda denna funktion är:

```Elm
String.slice 5 10 "Hello World"
```

Output: `" World"`

I det här fallet skulle substrängen `" World"` extraheras från den större strängen `"Hello World"`.

## Djupdykning
När du extraherar substrängar i Elm är det viktigt att tänka på indexeringen. Det första tecknet i en sträng har index 0, vilket innebär att det andra tecknet har index 1, och så vidare. Om du vill extrahera de första fem tecknen i en sträng skulle `start` vara 0 och `end` skulle vara 5.

En annan viktig sak att notera är att `end` är exklusivt, vilket innebär att det kommer att inkludera tecknet på indexet `end-1`. Till exempel, om `end` är 8, kommer det åttonde tecknet att ingå i substrängen. Det är därför som i vårt exempel tidigare extraherade vi tecknen på index 5 till 9, men eftersom 9 är exklusivt inkluderar det tecknet på index 8, vilket ger oss en total på 5 tecken.

Det finns också andra funktioner som kan hjälpa dig att manipulera substrängar, till exempel `String.take` och `String.drop`, som tar ett argument för antalet tecknen som du vill ta eller släppa från början av en sträng. Det finns också andra funktioner som hjälper till med sökning och ersättning av text.

## Se även
- Officiell dokumentation för Elm's `String` modul: https://package.elm-lang.org/packages/elm/core/latest/String
- En guide för att arbeta med strängar i Elm: https://guide.elm-lang.org/strings/
- En interaktiv lektion om strängmanipulering i Elm: https://elmprogramming.com/string-manipulation-in-elm.html