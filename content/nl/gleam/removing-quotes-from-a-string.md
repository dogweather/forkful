---
title:                "Quotes verwijderen uit een string"
date:                  2024-01-28T22:06:09.259658-07:00
model:                 gpt-4-0125-preview
simple_title:         "Quotes verwijderen uit een string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verwijderen van aanhalingstekens uit een tekenreeks betekent het wegpellen van die extra lagen – de aanhalingstekens – van uw tekstgegevens. Programmeurs doen dit om invoer te saneren, tekenreeksen voor te bereiden voor verwerking of gewoon om dingen netjes en consistent te houden in hun applicaties. Uiteindelijk gaat het allemaal om schone, bruikbare gegevens.

## Hoe doe je dat:
Aanhalingstekens verwijderen in Gleam is eenvoudig. We kunnen patroonherkenning of ingebouwde tekenreeksfuncties gebruiken. Hier is een snel voorbeeld ter illustratie:

```gleam
pub fn remove_quotes(text: String) -> String {
  let without_quotes = string.trim(text, "\"")
  without_quotes
}

pub fn main() {
  let text_with_quotes = "\"Hallo, Wereld!\""
  let cleaned_text = remove_quotes(text_with_quotes)
  io.println(cleaned_text)
}
```

Voorbeeld van uitvoer:
```
Hallo, Wereld!
```

## Diepgaande duik
Historisch gezien is het omgaan met aanhalingstekens in tekenreeksen een veelvoorkomende taak geweest in tekstverwerking en scripttalen. Gezien de aard van tekenreeksen vaak gebruikersinvoer zijn of uit bestanden worden gelezen, kunnen ze worden geleverd met aanhalingstekens die om verschillende redenen verwijderd moeten worden, zoals database-invoeging of opmaak.

In Gleam gebruiken we de functie `string.trim` om de aanhalingstekens af te scheren. Er zijn alternatieven! We zouden door de tekenreeks kunnen loopen of reguliere expressies toepassen, maar `string.trim` is uw handige hulpmiddel voor de klus vanwege de beknopte en prestaties.

Als we duiken in implementatiedetails, werkt `string.trim` door karakters te verwijderen van het begin en einde van de tekenreeks die overeenkomen met het opgegeven patroon. Dus als u aanhalingstekens aan beide uiteinden van uw tekenreeks hebt, worden ze in één keer afgehakt. Houd er rekening mee dat het alleen de aanhalingstekens verwijdert als ze aan de randen zitten; aanhalingstekens die comfortabel in het midden van uw tekst zitten, blijven staan.

## Zie ook
Voor de nieuwsgierige geesten die meer willen verkennen:
- [Documentatie van Gleam's String-module](https://gleam.run/stdlib/string/)
- Discussies over tekstverwerking in programmering op [Stack Overflow](https://stackoverflow.com/questions/tagged/text-processing)
