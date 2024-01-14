---
title:                "Ruby: Ekstrahering av delstrenger"
simple_title:         "Ekstrahering av delstrenger"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noen gang har jobbet med tekstbehandling i Ruby, har du sannsynligvis kommet over behovet for å hente ut deler av en tekststreng. Dette kan være for å formatere data, lage variabler eller behandle informasjon på en mer effektiv måte. Uansett årsak, er det nyttig å vite hvordan man skal trekke ut substrings i Ruby. 

## Hvordan
Det er flere måter å ekstrahere substrings i Ruby, men de mest vanlige metodene er `slice`, `substring` og `scan`. La oss se på noen eksempler på hvordan disse metodene fungerer og hva slags utgang de produserer. 

```Ruby
sentence = "Å programmere i Ruby er gøy!"
puts sentence.slice(2, 9) # Output: # "programmere"
puts sentence.substring(3, 5) # Output: "program"
puts sentence.scan(/^[^\s]+/) # Output: ["Å"]
``` 

For å bruke `slice`-metoden trenger vi å kjenne posisjonen til start- og sluttsymbolet for substringsen vi vil hente ut. I eksempelet ovenfor bruker vi `2` som startposisjon og `9` som sluttposisjon for å hente ut ordet "programmere". 

For `substring`-metoden må vi også spesifisere start- og sluttposisjoner, men i dette tilfellet må vi også angi hvor mange tegn vi vil hente ut. I eksempelet ovenfor angir vi at vi vil ha ut 5 tegn fra og med tredje posisjon, som gir oss substringen "program". 

Til slutt har vi metoden `scan`, som lar oss hente ut substrings basert på et mønster. I dette tilfellet ønsker vi å hente ut ordet som kommer før det første mellomrommet i setningen. Ved å bruke et regulært uttrykk, som i eksempelet ovenfor, kan vi lett hente ut "Å" som første ord i setningen. 

## Deep Dive
Det er viktig å merke seg at de ulike metodene for å ekstrahere substrings i Ruby kan ha forskjellige utgangspunkt for start- og sluttposisjoner. For eksempel bruker `substring`-metoden tegnnummer som utgangspunkt, mens `slice`-metoden bruker indekser fra `0`. Dette kan føre til uventede utganger hvis man ikke er klar over forskjellene. 

Et annet viktig punkt å merke seg er at begge metodene kan utvides ved å bruke åpne intervall, for eksempel `y..-1`, som betyr "fra og med y til slutten av strengen". Dette gjør disse metodene svært fleksible og nyttige for å håndtere variabel data. 

## Se Også
- [Ruby String class documentation](https://ruby-doc.org/core-2.7.2/String.html)
- [Ruby String#slice method documentation](https://ruby-doc.org/core-2.7.2/String.html#method-i-slice)
- [Ruby String#substring method documentation](https://ruby-doc.org/core-2.7.2/String.html#method-i-substring)