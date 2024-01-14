---
title:    "Gleam: Användning av reguljära uttryck"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Reguljära uttryck är ett kraftfullt verktyg för att söka, matcha och manipulera textsträngar i din Gleam-programmering. Genom att lära dig hur du använder reguljära uttryck kan du effektivt hantera stora mängder data och automatisera uppgifter.

## Så här gör du

För att använda reguljära uttryck i Gleam, behöver du importera modulen `Regex`. När modulen är importerad, kan du skapa ett reguljärt uttryck genom att använda funktionen `Regex.compile()` och passa in en sträng som beskriver mönstret du vill matcha. Till exempel, om du vill hitta alla förekomster av "programmering" i en textsträng kan du använda följande kod:

```Gleam
let regex = Regex.compile("programmering")
```

Nästa steg är att använda funktionen `Regex.find()` för att söka efter matchningar av mönstret i en textsträng. Om det finns en matchning, returnerar funktionen en tupel med matchningen och dess position i textsträngen. Om ingen matchning hittas, returnerar funktionen `none`. 

För att bättre förstå hur detta fungerar, låt oss titta på ett enkelt exempel. Antag att vi har en variabel `text` som innehåller följande sträng:

```Gleam
let text = "Jag älskar programmering! Gleam är mitt favoritspråk."
```

För att hitta alla förekomster av ordet "programmering" i denna text kan vi använda följande kod:

```Gleam
let regex = Regex.compile("programmering")
let match = Regex.find(text)

pub fn main() {
  case match {
    Just(result) -> io.print(result)
    Nothing -> io.println("Ingen matchning hittades")
  }
}
```

I detta exempel kommer funktionen `print()` att skriva ut matchningen "programmering" till konsolen. Om det inte finns någon matchning, kommer vi att få ett meddelande som säger "Ingen matchning hittades".

## Djupdykning

Reguljära uttryck kan vara mycket mer avancerade än bara att hitta en enkel sträng. Genom att använda olika metatecken och operatorer, kan du skapa mer dynamiska och precisa mönster för att matcha textsträngar. Till exempel kan du använda `^` för att matcha en sträng som börjar med ett visst mönster och `$` för att matcha en sträng som slutar med ett visst mönster.

En annan användbar funktion är `Regex.replace()`, som låter dig byta ut matchade mönster i en textsträng med en annan sträng. Detta kan vara användbart för att rensa upp formatering i text eller för att göra massaändringar i stora datamängder.

Det finns mycket mer att lära sig om reguljära uttryck, men dessa grundläggande koncept bör hjälpa dig att komma igång.

## Se även

- [Gleam dokumentation om reguljära uttryck](https://gleam.run/modules/regex.html)
- [Ett interaktivt reguljärt uttryck-verktyg](https://regexr.com/)