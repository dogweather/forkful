---
title:    "Ruby: Slette tegn som matcher et mønster"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger i programmering kan man komme over situasjoner der man trenger å fjerne visse tegn fra en tekststreng. Dette kan være for å rydde opp i data eller for å filtrere ut uønsket informasjon. Ved å lære å slette tegn som matcher et mønster, kan man effektivt håndtere slike situasjoner.

## Hvordan gjøre det

For å slette tegn som matcher et mønster i Ruby, kan man bruke metoden `.gsub!` som står for "global substitution". Denne metoden tar imot to argumenter: det første er mønsteret som skal matches, og det andre er hva som skal erstatte det matchede mønsteret.

La oss si at vi har en variabel `tekst` som inneholder følgende tekststreng: "Jeg elsker å spise sushi!". Hvis vi ønsker å fjerne alle vokaler fra denne teksten, kan vi bruke `.gsub!` som følger:

```Ruby
tekst.gsub!(/[aeiou]/i, "")
```

Her angir vi mønsteret `[aeiou]`, som betyr alle små eller store vokaler, og det tomme strengsargumentet betyr at vi vil erstatte disse vokalene med ingenting. Outputen vil da bli:

`Jg lskr å spr ssh!`

Hvis vi ønsker å erstatte disse vokalene med et annet tegn, for eksempel en stjerne, kan vi gjøre det ved å endre det andre argumentet til `*`.

```Ruby
tekst.gsub!(/[aeiou]/i, "*")
```

Nå vil outputen bli:

`J*g*l*v * *sp*s* s*sh*!`

## Dypdykk

I eksemplene ovenfor brukte vi `/.gsub!`-metoden med metakarakteret `i` for å gjøre søket case-insensitive. Dette betyr at den også vil matche store bokstaver. Hvis man ønsker å begrense søket til kun små bokstaver, kan man utelate metakarakteret `i`.

Man kan også bruke `.gsub`-metoden i stedet for `.gsub!`. Forskjellen er at `.gsub` vil returnere en ny tekststreng med endringene, mens `.gsub!` vil endre den eksisterende tekststrengen direkte.

Det er også mulig å bruke andre mønstre, for eksempel `[0-9]` for tall eller `[!@#$%^&*()]` for spesialtegn.

## Se også

- Ruby regex tutorial: https://www.rubyguides.com/2015/06/ruby-regex/
- Ruby String class documentation: https://ruby-doc.org/core-2.7.2/String.html
- Regular Expressions cheat sheet: https://www.shortcutfoo.com/app/dojos/ruby-regex/cheatsheet