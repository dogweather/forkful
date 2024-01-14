---
title:    "Elixir: Radera tecken som matchar ett mönster."
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Varför: 

Det kan finnas många olika orsaker till varför man vill ta bort karaktärer som matchar ett mönster i sin Elixir-kod. Det kan vara för att rensa upp i en sträng, filtrera ut oönskade karaktärer eller för att bygga ett effektivt pattern-matching-system. Oavsett anledning är det viktigt att förstå hur man kan göra detta för att förbättra sin kod.

##Så här gör du:

För att ta bort karaktärer som matchar ett mönster i Elixir kan man använda sig av funktionen `String.replace/4`. Första argumentet är strängen som ska bearbetas, andra argumentet är mönstret som man vill matcha, tredje argumentet är den ersättande strängen och det fjärde argumentet är antalet gånger som man vill ersätta mönstret (standardvärdet är alla förekomster). Nedan följer ett exempel på hur man kan använda denna funktion:

```Elixir
sträng = "Hej! Välkommen till min blogg!"
mönster = ~r/!/
ersätt_med = ""
antal = 1 
String.replace(sträng, mönster, ersätt_med, antal)
```

Detta kommer att ge utmatningen: `"Hej Välkommen till min blogg!"`. Vi har här ersatt första förekomsten av tecknet "!" med en tom sträng.

##Djupdykning:

När man arbetar med pattern-matching i Elixir är det viktigt att förstå hur regelbundna uttryck fungerar. Man kan använda sig av symbolen "~r/" för att skapa ett regelbundet uttryck och sedan ange ett mönster som ska matchas. I exemplet ovan använde vi oss av mönstret "~r/!/" för att matcha förekomsten av tecknet "!". Man kan också använda sig av modifierare som t.ex. "i" för att göra sökningen fall-ignorerande.

Det finns även möjlighet att använda sig av `Regex.run/3` för att få tillbaka en lista med den matchade strängen och alla grupper som matchar. Detta kan vara användbart om man vill ha mer kontroll över hur man hanterar resultatet av sökningen.

##Se även:

- Elixir dokumentation för `String.replace/4`: https://hexdocs.pm/elixir/String.html#replace/4
- Elixir dokumentation för regelbundnauttryck: https://hexdocs.pm/elixir/Regex.html 
- Elixir dokumentation för `Regex.run/3`: https://hexdocs.pm/elixir/Regex.html#run/3