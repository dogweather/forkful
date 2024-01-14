---
title:    "Gleam: Stora bokstäver i en rad"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kapitalisera en sträng är en vanlig uppgift inom många programmeringsspråk och kan vara användbart för att ändra utseendet på textsträngar. Detta kan vara särskilt användbart när man vill formatera utskrifter eller jämföra strängar med olika versaler och gemener.

## Så här gör man

För att kapitalisera en sträng i Gleam behöver man använda funktionen `capitalize` från standardbiblioteket `String`. Här är ett enkelt exempel:

```Gleam
import String

String.capitalize("hej") // resultat: "Hej"
```

Om man vill ändra en hel mening eller en längre sträng kan man använda funktionen `map` för att applicera `capitalize` på varje ord. Ett exempel:

```Gleam
import String
import List

let text =
    "det här är en mening som ska kapitaliseras"

List.map(text, String.capitalize) // resultat: ["Det", "Här", "Är", "En", "Mening", "Som", "Ska", "Kapitaliseras"]
```

Man kan även använda den här tekniken för att jämföra två strängar oberoende av versaler och gemener. Genom att först kapitalisera båda strängarna kan man sedan jämföra dem på ett mer effektivt sätt.

## Djupdykning

I Gleam så blir alla tecken i en sträng mellan versal `A` och den ensamma versalen `å` betraktade som gemener. Det betyder att alla andra versaler så som `Ä`, `Ö` och `Å` behandlas som gemener och kommer inte att förändras av funktionen `capitalize`. Om man vill behandla alla versaler på samma sätt behöver man ta hänsyn till detta genom att eventuellt använda en annan metod eller funktion.

## Se även

- [Gleams officiella dokumentation för strängar](https://gleam.run/documentation/0.13/standard-libraries#string)
- [En guide om hur jämföra strängar i Gleam](https://dev.to/yogesnsamy/how-to-compare-strings-in-gleam-3omg)