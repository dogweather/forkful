---
title:                "Ruby: Konvertera en sträng till gemener"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver kan vara användbart när du behöver jämföra strängar utan att oroa dig för stora och små bokstäver. Det kan också bidra till enhetlighet och läsbarhet i din kod. 

## Hur man gör

För att konvertera en sträng till små bokstäver i Ruby, använd metoden `downcase`. Till exempel:

```Ruby
str = "Hej, DET HÄR ÄR EN STRÄNG"
puts str.downcase
```

Den här koden kommer att producera outputen "hej, det här är en sträng". Som du kan se, har alla bokstäver konverterats till små bokstäver. 

## Djupdykning

När du använder `downcase`-metoden för en sträng, kommer alla bokstäver A-Z att konverteras till a-z (detta inkluderar även speciella tecken som Å, Ä och Ö i den svenska alfabetet). Siffror, symboler och mellanslag påverkas inte och kommer fortfarande att vara oförändrade i outputen.

Något att tänka på är att metoden `downcase` inte kommer att ändra den ursprungliga strängen, utan returnerar en ny sträng med de små bokstäverna. Om du vill spara den nya strängen måste du därför tilldela den till en variabel, som i exemplet ovan.

## Se också

För mer information om strängbehandling i Ruby, se följande länkar:

- [Ruby - Strings](https://www.rubyguides.com/ruby-tutorial/ruby-strings/)
- [The Ruby String Class](https://www.rubyguides.com/2019/03/ruby-string-class/)