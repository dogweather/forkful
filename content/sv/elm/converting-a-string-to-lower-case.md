---
title:                "Omvandla en sträng till gemener"
html_title:           "Elm: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till gemener (lower case) är en vanlig operation inom programmering, särskilt när man hanterar användarinput eller jämför strängar. Genom att lära dig hur man gör detta i Elm kan du öka din kodningskompetens och effektivitet.

## Så här gör du
Det finns flera sätt att konvertera en sträng till gemener i Elm, beroende på hur du behöver använda den. Här är några exempel på olika metoder:

```Elm
-- Använd String.toLower för att konvertera en sträng till gemener
String.toLower "ELM"   -- ger "elm"

-- Du kan också använda List.map och Char.toLower för att konvertera varje bokstav
String.toList "ELM" |> List.map Char.toLower |> String.fromList   -- ger "elm"

-- Om du vill konvertera endast den första bokstaven kan du använda Char.toLower för den första bokstaven och sedan lägga till resten av strängen
Char.toLower (String.head "ELM")   -- ger "e"
Char.toLower (String.head "ELM") :: String.tail "ELM"   -- ger "elm"
```

Det finns också olika bibliotek som du kan importera för att hjälpa dig att konvertera strängar till gemener, som till exempel [elm-case](https://package.elm-lang.org/packages/AlexanderFlood/elm-case/latest/).

## Djupdykning
När man konverterar en sträng till gemener är det viktigt att vara medveten om att bokstäverna inte alltid blir exakt som förväntat. Till exempel kan bokstaven "İ" i turkiska inte konverteras till "i" eftersom de inte är ekvivalenta. Det är också bra att vara medveten om att vissa språk som kinesiska och japanska inte har skillnader mellan gemener och versaler, vilket innebär att dessa operationer inte har någon effekt på deras alfabet.

En annan viktig punkt att tänka på är att Elm är ett språk som värnar om typsäkerhet, vilket innebär att det kan vara oväntade resultat om du försöker använda samma metod för att konvertera strängar av olika datatyper, som till exempel en Integer eller en Float.

## Se också
- [elm-case](https://package.elm-lang.org/packages/AlexanderFlood/elm-case/latest/)
- [String.toLower - Elm Docs](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [List.map - Elm Docs](https://package.elm-lang.org/packages/elm/core/latest/List#map)
- [Char.toLower - Elm Docs](https://package.elm-lang.org/packages/elm/core/latest/Char#toLower)