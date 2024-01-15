---
title:                "Sökning och ersättning av text"
html_title:           "Haskell: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Varför

Att söka och ersätta text är en vanlig uppgift i programmering, speciellt när man jobbar med stora mängder data eller textfiler. Oavsett om du behöver ändra stavfel eller ersätta vissa ord med andra, kan Haskell göra detta processen enklare och mer effektiv.

# Hur man gör det

För att söka och ersätta text i Haskell behöver du använda en funktion som heter `substitute`. Denna funktion tar tre argument: det första är det du vill söka efter, det andra är det du vill ersätta det med och det tredje är den texten du vill söka igenom. Här är ett exempel på hur du kan använda `substitute`:

```Haskell
substitute "bilar" "cyklar" "Jag gillar bilar"
```

I det här exemplet kommer `substitute` att byta ut alla instanser av "bilar" med "cyklar" i strängen "Jag gillar bilar". Resultatet blir "Jag gillar cyklar". Du kan också använda variabler istället för statiska strängar i funktionen, till exempel:

```Haskell
let word1 = "bilar"
let word2 = "cyklar"
let sentence = "Jag gillar bilar"
substitute word1 word2 sentence
```

Om du vill söka igenom en hel textfil kan du använda funktionen `substituteFile`. Denna funktion tar också tre argument, det första är söktermen, det andra är ersättningstermen och det tredje är namnet på textfilen. Här är ett exempel:

```Haskell
substituteFile "hund" "katt" "minFil.txt"
```

Detta kommer att byta ut alla instanser av "hund" med "katt" i textfilen "minFil.txt".

# Deep Dive

Förutom `substitute` och `substituteFile` finns det också andra funktioner i Haskell som kan hjälpa dig med sökning och ersättning av text. Till exempel finns det en funktion som heter `replace` som tar två argument: det första är söktermen och det andra är en lista med ersättningstermer. Här är ett exempel:

```Haskell
replace "hej" ["hello", "hi"] "Hej där"
```

Resultatet av detta kommer att vara en lista med "hello there" och "hi there". Du kan också använda `replace` på en hel textfil med funktionen `replaceFile`.

För mer komplicerade sökningar och ersättningar kan du använda reguljära uttryck med funktionen `substituteRegex`. Denna funktion tar tre argument: det första är det reguljära uttrycket för sökningen, det andra är den ersättningssträngen och det tredje är den texten som ska sökas igenom. Här är ett exempel:

```Haskell
substituteRegex "[A-Z]" " " "Hej Där"
```

I detta exempel kommer alla stora bokstäver att bytas ut med ett mellanslag, vilket ger "hej där" som resultat.

# Se också

För mer information och användbara funktioner för sökning och ersättning i Haskell, se följande resurser:

- [Haskell.org](https://www.haskell.org/): Officiell webbplats för Haskell språket.
- [Hoogle](https://hoogle.haskell.org/): En sökmotor för Haskell dokumentation och funktioner.
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters): En gratis onlinebok om Haskell.