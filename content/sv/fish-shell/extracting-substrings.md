---
title:                "Extrahering av delsträngar"
html_title:           "Fish Shell: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Substring-extrahering är en vanlig uppgift vid programmering, särskilt när du hanterar textdata. Det kan hjälpa dig att effektivt manipulera och hantera textsträngar enligt specifika kriterier.

## Hur man gör det

Det finns många olika sätt att extrahera substrings i Fish Shell, men vi kommer att fokusera på två vanliga metoder: användning av inbyggda kommandon och användning av reguljära uttryck.

Först ska vi se på hur man använder inbyggda kommandon för att extrahera substrings:
```
Fish Shell
# Spara textsträngen i en variabel
set sträng "Hej världen"

# Extrahera de första 3 tecknen
string sub --start 0 --length 3 $sträng
# Resultat: "Hej"

# Extrahera tecknen mellan index 4 och 8
string sub --start 4 --length 4 $sträng
# Resultat: "värld"

# Extrahera de sista 5 tecknen
string sub --length -5 $sträng
# Resultat: "världen"
```

Nu ska vi titta på hur man använder reguljära uttryck för att extrahera substrings:
```
Fish Shell
# Spara textsträngen i en variabel
set sträng "Detta är en text"

# Extrahera första ordet (allt innan första mellanslaget)
string match -r "(\w+)" $sträng
# Resultat: "Detta"

# Extrahera alla ord som börjar med bokstaven "t"
string match -rx "t\w+" $sträng
# Resultat: "text"

# Extrahera alla tecken mellan "är" och "en"
string match -r "är (\w+) en" $sträng
# Resultat: "en"
```

## Djupgående

Det finns många fler alternativ och möjligheter när det gäller substring-extrahering i Fish Shell. Det är värt att utforska olika kommandon och tekniker för att hitta vad som fungerar bäst för ditt specifika projekt. En annan användbar funktion är "string replace", som låter dig byta ut en del av en textsträng med en annan. Du kan också använda "string split" för att dela upp en sträng baserat på ett givet tecken eller mönster.

## Se även

- Fish Shell-dokumentation om "string" kommandon: https://fishshell.com/docs/current/cmds/string.html
- Tutorial om reguljära uttryck: https://www.regular-expressions.info/tutorial.html
- Bevaka String Extraction i Bash Shell: https://usefulangle.com/post/78/string-extraction-bash-shell