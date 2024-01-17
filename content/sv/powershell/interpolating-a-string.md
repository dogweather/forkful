---
title:                "Interpolera en sträng"
html_title:           "PowerShell: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att interpolera en sträng innebär att man inkluderar variabler eller uttryck i en textsträng. Detta gör det lättare och mer flexibelt att skapa dynamiska strängar med variabelt innehåll. Programerare använder detta för att till exempel skriva ut resultatet av beräkningar eller visa information från användare i ett meddelande.

## Hur gör man?

Det finns flera sätt att interpolera strängar i PowerShell. Det enklaste sättet är att använda en $-tecken följt av variabelnamnet eller uttrycket som önskas inkluderas i strängen. Exempelvis:

```PowerShell
$namn = "Lisa"
$"Hej $namn, välkommen!"
```

Detta kommer att resultera i att "Hej Lisa, välkommen!" skrivs ut i konsolfönstret. Man kan även använda operatorn `-f` för att interpolera strängar, där variabler eller uttryck placeras inom klammerparenteser och separeras med kommatecken. Exempel:

```PowerShell
$förnamn = "Karl"
$efternamn = "Johansson"
"Välkommen {0}, {1}!" -f $efternamn, $förnamn
```

Detta kommer att skriva ut "Välkommen Johansson, Karl!".

## Djupdykning

Interpolation av strängar är en vanlig teknik inom programmering och har funnits sedan tidigt 1970-tal. Det finns även alternativa sätt att inkludera variabler i strängar, som till exempel konkatenering, där variabler sätts ihop med hjälp av operatorn `+`. Detta kan dock göra koden svårläst och mindre effektiv jämfört med stränginterpolation.

I PowerShell finns även möjligheten att använda sig av utvidgad stränginterpolation, som innebär att man kan inkludera uttryck som beräknas inuti strängen. Detta görs genom att placera uttrycken inom parenteser och följa dessa med operatorn `-is` följt av en `s` för att indikera att uttrycken ska evalueras som strängar. Exempel:

```PowerShell
$antal = 5
"Jag har totalt $(($antal * 2) - 5)s äpplen kvar."
```

Detta kommer att resultera i att "Jag har totalt 5 äpplen kvar." skrivs ut.

## Se även

För mer information om interpolering av strängar i PowerShell kan du läsa dokumentationen på Microsofts hemsida: https://docs.microsoft.com/sv-se/powershell/scripting/samples/creating-slide-show.aspx