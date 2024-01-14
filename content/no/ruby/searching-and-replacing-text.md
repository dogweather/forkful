---
title:    "Ruby: Søke og erstatte tekst"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en viktig ferdighet å ha som programmerer. Det tillater oss å gjøre endringer i store mengder av tekst med bare noen få kommandoer, noe som sparer oss for tid og manuell arbeid. Uansett om du arbeider med et stort prosjekt eller bare trenger å endre noen få ord, vil du lære å søke og erstatte være en svært verdifull ferdighet.

## Slik gjør du det

Søking og erstatting av tekst i Ruby kan gjøres enkelt ved hjelp av innebygde metoder. Her er en rask guide til å søke og erstatte i en enkelt streng:
```
str = "Hei, dette er en tekst som skal endres."
puts str.gsub("endres", "byttes ut")
```
Output: "Hei, dette er en tekst som skal byttes ut."

Hvis du ønsker å søke og erstatte i flere strenger, kan du bruke en løkke:
```
tekster = ["Første tekst", "Andre tekst", "Tredje tekst"]

tekster.each do |tekst|
    puts tekst.gsub("tekst", "ord")
end
```
Output:
"Første ord"
"Andre ord"
"Tredje ord"

For mer komplekse søk og erstattinger, kan du bruke regulære uttrykk. I dette eksempelet erstatter vi alle tall med "X":
```
str = "12345, 67890"
puts str.gsub(/\d/, "X")
```
Output: "XXXXX, XXXXX"

## Dypdykk

Når du utfører søk og erstatting i store tekstmengder, er det viktig å være bevisst på hvilke deler av teksten du endrer. Hvis du bare ønsker å endre en del av teksten, kan du bruke regulære uttrykk og spesifikke søkeord for å sikre at du ikke gjør utilsiktede endringer.

Det er også mulig å kombinere flere søk og erstattinger på en enkelt linje ved hjelp av metoden `gsub!` som gjør endringene direkte i den opprinnelige variabelen.

## Se også

- [Ruby dokumentasjon: `gsub` og `gsub!` metoder](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Regulære uttrykk guide for Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Søk og erstatte i tekst med Ruby video tutorial](https://www.youtube.com/watch?v=QHQ2DEOUMe8)