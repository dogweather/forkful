---
title:                "Ruby: Extrahering av delsträngar"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Ibland behöver vi arbeta med strängar i vårt Ruby-programmeringsprojekt. Ibland kan vi behöva ta ut en del av en sträng, också kallat en substring, för att manipulera eller jämföra den med en annan sträng. Att extrahera substrings är en användbar teknik som kan spara mycket tid och arbete. Det är också ett viktigt koncept att förstå för att kunna arbeta med strängar på ett effektivt sätt.

## Såhär gör du

För att extrahera en substring från en sträng kan vi använda metoden `slice` eller `[index]` syntaxen.

```
# Definiera en sträng för att arbeta med
sträng = "Ruby är ett fantastiskt programmeringsspråk!"

# Använda slice metoden för att extrahera en del av strängen
sträng.slice(5, 15) # Returnerar "ett fantastiskt"

# Använda [index] syntaxen för att extrahera en del av strängen
sträng[20..-1] # Returnerar "fantastiskt programmeringsspråk!"
```

Vi kan också använda `match` metoden tillsammans med reguljära uttryck för att hitta en substring i en större sträng.

```
# Definiera en sträng för att arbeta med
sträng = "Ruby är ett fantastiskt programmeringsspråk!"

# Använda match metoden för att hitta en substring
sträng.match(/fantastiskt/) # Returnerar en MatchData objektet för "fantastiskt"
```

Det är också viktigt att notera att Ruby har en inbyggd klass `StringScanner` som kan hjälpa till med mer avancerad substring extrahering. Genom att använda denna klass kan vi hitta och manipulera olika delar av en sträng baserat på angivna mönster.

## Djupdykning

När man extraherar substrings är det viktigt att förstå hur indexeringen av strängar fungerar i Ruby. Strängar i Ruby är noll-indexerade, vilket betyder att första tecknet i en sträng har index 0. Detta är viktigt att komma ihåg när vi använder `slice` metoden eller `[index]` syntaxen för att extrahera substrings.

Vi kan också använda metoden `split` för att dela en sträng i flera delar och sedan använda olika delar av den som substrings. Regular expressions är också ett kraftfullt verktyg när man arbetar med substrings, eftersom de låter oss hitta mönster i en sträng snabbt och enkelt.

## Se även

- [Ruby dokumentationen för strängar](https://ruby-doc.org/core-2.7.2/String.html)
- [Tutorial om att arbeta med strängar i Ruby](https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-ruby)
- [Mer information om Ruby's StringScanner klass](https://ruby-doc.org/stdlib-2.7.2/libdoc/strscan/rdoc/StringScanner.html)