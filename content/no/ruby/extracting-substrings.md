---
title:    "Ruby: Uthenting av delstrenger"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor 

I Ruby programmering, er det ofte nødvendig å kunne hente ut en del av en tekststreng. Denne prosessen kalles "substring extraction" på engelsk, og det kan være nyttig å kunne for å manipulere tekststrenger eller hente ut spesifikke deler av en streng. I denne bloggartikkelen, vil jeg gå gjennom hvordan man kan utføre substring extraction i Ruby.

## Slik gjør du det

For å hente ut en substring i Ruby, kan man bruke metoden `slice` eller `[]` med tekststrengen som man ønsker å hente ut en del av. La oss si at vi har en tekststreng som heter `tekststreng` og vi ønsker å hente ut de tre første bokstavene. Da kan vi bruke følgende kode:

```Ruby
tekststreng = "Dette er en tekststreng"
puts tekststreng.slice(0,3) # Output: "Det"
```

I dette eksempelet har vi brukt `slice` metoden for å spesifisere at vi ønsker å hente ut karakterene fra indeks 0 til indeks 3. Dette betyr de første tre karakterene i tekststrengen. Vi kan også bruke `[]` metoden på samme måte:

```Ruby
tekststreng = "Dette er en tekststreng"
puts tekststreng[0,3] # Output: "Det"
```

Det er viktig å merke seg at Ruby bruker null-indeks, som betyr at den første karakteren i tekststrengen har indeks 0.

## Dypdykk

For å være mer presis, er `slice` metoden faktisk en alias for `[]`, så de gjør akkurat det samme. Man kan derfor bruke den ene eller den andre avhengig av personlige preferanser. Det som skiller disse to metodene fra hverandre er at `slice` kan ta et annet argument for å specificere lengden på substringen, i tillegg til startposisjonen. Dette er nyttig hvis man ønsker å hente ut en lengre del av en tekststreng. For eksempel:

```Ruby
tekststreng = "Dette er en tekststreng"
puts tekststreng.slice(5,7) # Output: "er en t"
```

Her har vi spesifisert at vi ønsker å hente ut 7 karakterer fra indeks 5, som betyr de ordene som starter på 'er'. Dette er spesielt nyttig hvis man ønsker å hente ut en del av en tekststreng basert på ord istedenfor karakterer.

## Se også

- https://www.rubyguides.com/2015/09/ruby-string-methods/#Extracting-a-Range
- https://learnruby.com/about-ruby-for-newbies/substrings/