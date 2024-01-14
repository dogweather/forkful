---
title:                "Bash: Extrahera delsträngar."
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en vanlig uppgift inom Bash-programmering. Det kan hjälpa dig att manipulera strängar och få ut den del av en sträng som du behöver för att lösa ett problem. Det är ett användbart verktyg att ha i verktygslådan för alla som arbetar med Bash.

## Så här gör du

För att extrahera en substring i Bash, kan du använda kommandot `cut` eller `grep`. Låt oss titta på ett exempel:

```Bash
str="Hej alla! Välkomna till min blogg." # Skapar en variabel med en lång sträng
echo ${str:4:4}  # Skriver ut en substring som börjar på position 4 och är 4 tecken lång
```

Output: `alla`

Här använde vi parameterexpansion för att extrahera en substring från en variabel. Syntaxen är `${var:start:length}`, där `start` är positionen där substrängen ska börja och `length` är längden på den extraherade substringen.

Om du vill extrahera en del av en sträng baserat på ett visst mönster kan du använda `cut` eller `grep`. Låt oss ta en titt på ett exempel med `grep`:

```Bash
str="Det finns många äpplen i korgen." # Skapar en variabel med en sträng
echo ${str | grep -o "många.*kor"}  # Skriver ut en substring som matchar mönstret "många.*kor"
```

Output: `många äpplen i korgen`

Här använder vi parametern `-o` för att bara skriva ut den matchande delen av strängen och mönstret `många.*kor` för att välja den del av strängen vi är intresserade av.

## Djupdykning

Nu när vi har en grundläggande förståelse för hur man extraherar substrängar i Bash, låt oss titta på några andra användbara funktioner och tekniker för att göra det.

En viktig sak att tänka på är att inledningen av en sträng är position 0, vilket ofta kan orsaka förvirring och misstag. Så om du vill extrahera de första 4 tecknen av en sträng, måste du ange `${var:0:4}` istället för `${var:1:4}`.

En annan användbar teknik är att använda parameterexpansion för att få ut en del av en sträng baserat på ett visst mönster. Till exempel, om du vill extrahera en del av en URL som innehåller en viss term, kan du använda följande syntax: `${var#*sökterm}`. Detta kommer att ta bort allt som kommer före söktermen i strängen.

## Se även

- [GNU Bash manual](https://www.gnu.org/software/bash/manual/bash.html) - Här hittar du mer information om parameterexpansion och andra användbara funktioner inom Bash.
- [Regex101](https://regex101.com/) - En användbar webbplats för att testa dina regex-mönster.