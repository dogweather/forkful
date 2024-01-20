---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som passer til et mønster er en programmeringsoppgave som handler om å fjerne bestemte deler av en streng. Programmere gjør dette for å tilpasse data, rense opp i uønskede tegn, eller for å bryte ned informasjon til en mer håndterlig form.

## Hvordan:
Her er et eksempel på hvordan du kan slette bokstaver fra en streng i Ruby. Anta at du vil fjerne alle vokaler fra en tekststreng. Da kan du bruke `delete` metoden:

```ruby
tekst = 'Hei, hvordan går det?'
resultat = tekst.delete 'aeiouyæøå'
puts resultat
```

Dette vil skrive ut: 'H, hvrdn gr dt?'.

Metoden `delete` tar et sett med tegn og fjerner alle forekomstene av disse tegnene fra streng.

## Dyp Dykk
Å slette tegn som passer til et mønster er en felles oppgave i mange programmeringsspråk. Ruby-metoden `delete` har en historie som strekker seg tilbake til de tidlige dagene av Perl, som har et veldig lignende `tr///` operatør.

En alternativ metode ville være å bruke en regular uttrykk for å finne og erstatte tegn. Her er et eksempel på hvordan du kan gjøre det:

```ruby
tekst = 'Hei, hvordan går det?'
resultat = tekst.gsub(/[aeiouyæøå]/, '')
puts resultat
```

Dette vil gi samme resultat som det første eksemplet. Men det er verdt å merke seg at `gsub` metoden kan være mer fleksibel enn `delete`, ettersom det tillater mer komplekse mønstre.

## Se Også:
Hvis du vil lære mer om strengmanipulasjon i Ruby, kan du sjekke ut disse linkene:

- Ruby Dokumentasjon om Strenger: [Link](https://ruby-doc.org/core/String.html)
- Ruby Monks module på Strenger: [Link](https://rubymonk.com/learning/books/1-ruby-primer/chapters/5-strings/lessons/31-string-basics)
- Stack Overflow spørsmål om å fjerne tegn fra strenger: [Link](https://stackoverflow.com/questions/14348494/remove-character-from-string-ruby)