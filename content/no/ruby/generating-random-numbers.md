---
title:                "Generering av tilfeldige tall"
html_title:           "Ruby: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Generering av tilfeldige tall er en vanlig praksis blant programmerere, spesielt innen spill og sikkerhet. Det betyr rett og slett å lage tall som ser ut til å være tilfeldige, men som egentlig følger et visst mønster. Dette brukes ofte til å skape variasjon i spill eller for å sikre at data er tilfeldig og vanskelig å forutsi.

# Hvordan?

For å generere tilfeldige tall i Ruby, kan vi bruke metoden "rand", som står for "random". Den tar inn et tall eller et tallområde som parameter, og returnerer et tilfeldig tall innenfor dette området. Her er et eksempel på hvordan du kan bruke den:

```ruby
# Genererer et tilfeldig heltall mellom 1 og 10
rand(1..10) # => 7

# Genererer et tilfeldig desimaltall mellom 0 og 1
rand # => 0.34637209981227
```

Som du kan se, kan vi gi "rand" en parameter eller la den generere et tall mellom 0 og 1 som standard. Dette er nyttig når vi trenger et tilfeldig tall i et bestemt område eller når vi bare trenger et tilfeldig tall for å lage variasjon i koden vår.

# Dypdykk

Historisk sett har den første metoden for å generere tilfeldige tall i programmering vært å bruke formler som følger et visst mønster. Disse ble ofte brukt i spill for å få "tilfeldige" resultater uten å måtte bruke faktisk tilfeldige tall. I dag er mange programmeringsspråk, inkludert Ruby, utstyrt med biblioteker som gjør det mulig å generere virkelig tilfeldige tall ved hjelp av eksterne enheter, som radioaktivitet eller mus-bevegelser.

Et alternativ til "rand"-metoden i Ruby er å bruke "SecureRandom"-biblioteket. Dette gir enda større sikkerhet i kryptografisk sensitive applikasjoner, da den bruker en sterkere tilfeldighetskilde enn bare å generere tall basert på formel.

# Se også

Hvis du vil lære mer om tilfeldige tall og hvordan de brukes i programmering, kan du sjekke ut disse kildene:

- Ruby sin offisielle dokumentasjon for "rand" og "SecureRandom" metoder: https://ruby-doc.org/core-2.6.2/Random.html
- En artikkel om tilfeldige tall i programmering og hvordan de brukes i forskjellige områder, som spill, sikkerhet og statistikk: https://en.wikipedia.org/wiki/Random_number_generation_in_programming
- En tutorial om hvordan du genererer tilfeldige tall i Ruby og hvordan du kan bruke dem i forskjellige situasjoner: https://www.tutorialspoint.com/ruby/ruby_random_numbers.htm