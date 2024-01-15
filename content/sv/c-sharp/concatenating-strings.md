---
title:                "Sammanfogning av strängar"
html_title:           "C#: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
Du kanske undrar varför det är nödvändigt att konkatenera strängar i C # -programmering. Svaret är enkelt - det ger dig möjligheten att kombinera flera strängar till en enda lång sträng. Detta kan vara användbart för att skapa dynamiska meddelanden eller för att bygga upp långa filvägar.

## Hur man gör det
Det finns flera sätt att konkatenera strängar i C #. Ett sätt är att använda "+" -operatorn, som lägger till två strängar tillsammans. Här är ett exempel på hur du kan använda detta:

```C#
string förnamn = "Lisa";
string efternamn = "Andersson";
string fullständigtNamn = förnamn + " " + efternamn; // fullständigtNamn kommer att bli "Lisa Andersson"
```

Ett annat sätt att konkatenera strängar är att använda String.Format-metoden. Detta gör att du kan skapa en mallsträng med platshållare och sedan ersätta dessa med dynamiska värden. Här är ett exempel:

```C#
string datum = "19 april";
string månad = "april";

string meddelande = String.Format("Idag är det {0}, den {0} är den {1} dagen i året.", datum, månad); // meddelande kommer att bli "Idag är det 19 april, den 19 april är den 109:e dagen i året."
```

Det finns också andra metoder som är inbyggda i C # för att konkatenera strängar, såsom String.Concat och StringBuilder-klassen.

## Djupdykning
När du konkatenerar strängar i C # är det viktigt att tänka på prestanda, särskilt om du behöver göra detta i en loop eller för en stor mängd data. Att använda "+" -operatorn kan leda till ineffektiv kod eftersom den skapar ett nytt objekt varje gång den används. En bättre lösning är att använda StringBuilder-klassen, som skapar en enda sträng och lägger till nya värden till den utan att behöva skapa en ny sträng varje gång.

Det är också viktigt att tänka på teckenkodning när du konkatenerar strängar. Om du blandar strängar med olika teckenkodningar, som Unicode och ASCII, kan det leda till oförutsägbara resultat.

## Se även
Här är några användbara resurser för dig att utforska om du vill lära dig mer om att konkatenera strängar i C #:

- [Microsofts dokumentation om strängkonkatenering](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/#concatenating-strings)
- [C # -konkateringsoperatörens prestanda jämförelse](https://mayukhsaha.wordpress.com/2011/08/20/performance-of-string-concatenation-in-c-net-a-comparative-analysis-with-operators-2/)
- [Konkatenera strängar med StringBuilder](https://www.geeksforgeeks.org/c-sharp-stringbuilder-append-method/)