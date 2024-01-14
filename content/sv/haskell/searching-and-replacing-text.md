---
title:    "Haskell: Söka och ersätta text"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en viktig del av programmering, då det tillåter oss att effektivt göra ändringar i stora mängder av text eller kod. Det är ett viktigt verktyg för att göra vårt arbete mer effektivt och för att undvika handgjorda ändringar som kan leda till misstag.

## Hur man gör det

Med Haskell kan vi enkelt söka och ersätta text med hjälp av inbyggda funktioner och moduler. Först måste vi importera "Data.Text" modulen, vilket ger oss tillgång till funktioner för att hantera textsträngar.

```Haskell
import Data.Text
```

Sedan kan vi använda funktionen "replace" för att söka och ersätta text i en given sträng. Denna funktion tar tre argument: söktermen, den nya texten och den ursprungliga strängen som ska ändras. Här är ett enkelt exempel där vi ersätter "Hej" med "Hej hej" i en given sträng:

```Haskell
sträng = "Hej, hur mår du?"
nySträng = replace "Hej" "Hej hej" sträng
print nySträng

-- Output: "Hej hej, hur mår du?"
```

Om vi vill göra flera ändringar i samma sträng kan vi använda "replaceEach" funktionen istället, som tar en lista av tupler som argument. Varje tupel innehåller söktermen och den nya texten. Här är ett exempel:

```Haskell
sträng = "God morgon världen!"
ersättningar = [("God", "Hej"), ("morgon", "kväll")]
nySträng = replaceEach ersättningar sträng
print nySträng

-- Output: "Hej kväll världen!"
```

## Djupdykning

I Haskell kan sök- och ändringsfunktionerna även användas på mer avancerade sätt, som till exempel med hjälp av reguljära uttryck. Med hjälp av "Data.Text.Regex" modulen kan vi använda funktioner som "subRegex" för att hitta och ersätta text baserat på mönster istället för exakta matchningar.

En annan användbar funktion är "replaceRegex", vilket tillåter oss att göra flera ersättningar i en sträng med enbart en funktion istället för att använda "replaceEach".

Genom att använda reguljära uttryck i sök- och ersättningsprocessen kan vi göra mer komplexa ändringar i texten och spara tid genom att inte behöva göra flera separata ändringar.

## Se även

- [Haskell.org](https://www.haskell.org/)
- [Data.Text modulen](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Data.Text.Regex modulen](https://hackage.haskell.org/package/regex-doc)