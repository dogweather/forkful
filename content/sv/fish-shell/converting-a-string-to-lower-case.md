---
title:                "Fish Shell: Att konvertera en sträng till små bokstäver"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener (lower case) är en vanlig uppgift inom programmering. Detta kan vara användbart när man hanterar användarinput, jämför strängar eller för att upprätthålla en enhetlig formatering i ett program. I denna bloggpost kommer jag att visa dig hur du enkelt kan göra detta med hjälp av Fish Shell.

## Så här gör du

För att konvertera en sträng till gemener i Fish Shell använder vi kommandot `string tolower`. Vi kan antingen använda detta direkt i vår terminal eller lägga till det i ett skript.

```Fish Shell
# Konvertera en sträng direkt i terminalen
string tolower "HEJ" 
# Resultat: hej

# I ett skript
#!/usr/local/bin/fish

set str "VÄLKOMMEN"
echo (string tolower $str) 
# Resultat: välkommen
```

Som du kan se i exemplet ovan används `string tolower` tillsammans med variabeln `str` för att hämta och konvertera värdet. Det resulterande värdet skrivs ut med hjälp av kommandot `echo`.

## Djupdykning 

En intressant detalj med `string tolower` är att den inte bara funktionerar på bokstäver inom det latinska alfabetet, utan även på specialtecken och diakritiska tecken. Detta betyder att du kan konvertera en sträng till gemener, oavsett vilket språk den är skriven på. Därför är det ett mycket användbart verktyg för internationell programmering.

### Hur fungerar det?

Vad som faktiskt händer bakom kulisserna är att `string tolower` använder sig av den inbyggda funktionen `string` tillsammans med `tolower` som parameter. Detta returnerar en modifierad version av strängen med alla bokstäver omvandlade till gemener.

## Se också

För mer information om Fish Shell och dess kommandon, kolla in följande länkar:

- [Officiell hemsida för Fish Shell](https://fishshell.com/)
- [Fiskkommandon och syntax](https://fishshell.com/docs/current/index.html#buitlinf)
- [Fish Shell på GitHub](https://github.com/fish-shell/fish-shell)

Tack för att du läste! Hoppas du har fått en bättre förståelse för hur man konverterar en sträng till gemener med hjälp av Fish Shell.