---
title:                "Ruby: Uttrekking av delstrenger"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor

Å ekstrahere substringer, eller deler av tekst, er en viktig del av programmering som kan hjelpe deg med å manipulere data på en mer effektiv måte. Enten det er å finne og erstatte bestemte deler av en tekst eller å hente ut spesifikke informasjon fra en større datamengde, kan ekstrahering av substringer bidra til å forenkle koden din og gjøre den mer lesbar.

# Hvordan gjøre det

For å ekstrahere substringer i Ruby, bruker vi metoden `slice` eller dens alias `[]`. Dette er en såkalt "mutator"-metode, som betyr at den endrer den originale teksten. La oss si at vi for eksempel har en tekst som sier "Hei, mitt navn er Ruby", og vi ønsker å ekstrahere bare ordet "Ruby". Vi kan gjøre det ved hjelp av følgende kode:

```Ruby
tekst = "Hei, mitt navn er Ruby"
puts tekst.slice(18..-1)
# output: Ruby
```

Vi bruker `slice`-metoden og gir den en parameter som sier hvilken del av teksten vi vil ha ut. I dette tilfellet vil `18..-1` hente ut alt fra og med det 18. tegnet (R) til slutten av teksten. Alternativt kunne vi også brukt `[]`-metoden på følgende måte:

```Ruby
tekst = "Hei, mitt navn er Ruby"
puts tekst[18..-1]
# output: Ruby
```

Dette er det samme som å bruke `slice`, ettersom `[]` er en alias for `slice`. Vi kan også bruke `[]`-metoden for å hente ut et bestemt antall tegn fra et gitt startpunkt:

```Ruby
tekst = "Hei, mitt navn er Ruby"
puts tekst[18, 4]
# output: Ruby
```

I dette tilfellet spesifiserer vi at vi vil hente ut 4 tegn fra og med det 18. tegnet. Ved å kombinere `slice`- eller `[]`-metoden med andre metoder som `gsub` og `chomp`, kan vi også manipulere tekst og erstatte substringer med annen informasjon.

# Dykk ned i detaljene

Ruby har også muligheten til å ekstrahere substringer ved hjelp av Regular Expressions, som er en mer avansert metode for å søke og manipulere tekst. Dette åpner for større fleksibilitet og muligheter når det gjelder å finne og erstatte spesifikke deler av en tekst. Det er også lurt å være oppmerksom på utfordringer som kan oppstå hvis vi prøver å ekstrahere substringer fra tekster som inneholder ulike tegnsett.

# Se også

- [Ruby dokumentasjon om `slice` og `[]`](https://ruby-doc.org/core-3.0.2/String.html#method-i-5B-5D)
- [Ruby Regular Expressions](https://ruby-doc.org/core-3.0.2/Regexp.html)