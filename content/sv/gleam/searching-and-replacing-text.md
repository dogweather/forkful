---
title:    "Gleam: Sökning och ersättning av text"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Varför

Att söka och ersätta text är en vanlig uppgift som många programmerare behöver utföra på daglig basis. Det kan vara för att ändra variabelnamn, uppdatera databasposter eller göra andra stora ändringar i kodbasen. Oavsett anledning är det viktigt att ha rätt verktyg för att effektivt söka och ersätta text.

# Hur man gör det

I Gleam kan du söka och ersätta text med hjälp av inbyggda funktioner som `str.replace` och `str.replace_all`. Dessa funktioner tar två argument: en söksträng och en ersättningssträng. Du kan också använda regelbundna uttryck för mer avancerade sökningar.

```Gleam
let original = "Hej! Välkommen till min blogg."
let nyText = str.replace(original, "min blogg", "Gleam-programmering")
io.print(nyText)

// Output: Hej! Välkommen till Gleam-programmering.
```

För att ersätta alla förekomster av en söksträng, använd `str.replace_all`:

```Gleam
let original = "Hej! Jag gillar choklad, choklad och mer choklad."
let nyText = str.replace_all(original, "choklad", "glass")
io.print(nyText)

// Output: Hej! Jag gillar glass, glass och mer glass.
```

# Deep Dive

Vid sökning och ersättning av text i Gleam, är det viktigt att förstå hur funktionerna fungerar. Både `str.replace` och `str.replace_all` returnerar en ny sträng med de ersatta förekomsterna. Det innebär att du behöver använda de nya värdena för att uppdatera variabler eller skriva ut dem till konsolen.

En annan viktig detalj är att båda funktionerna är case-sensitive, vilket betyder att de skiljer mellan små och stora bokstäver. Så om du söker efter "Hej" kommer inte "hej" att ersättas.

Det är också värt att nämna att båda funktionerna bara ersätter texten och returnerar inte en modifierad version av den ursprungliga strängen. Om du vill ersätta en del av en sträng måste det göras genom att skapa en ny variabel eller använda `str.insert` och `str.delete` för att manipulera strängen.

# Se även

- [Officiell Gleam dokumentation för textbehandling](https://gleam.run/documentation/standard-library/string/)
- [Gleam regex modul](https://gleam.run/documentation/stdlib/regex/)