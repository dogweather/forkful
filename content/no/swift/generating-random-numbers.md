---
title:                "Generering av tilfeldige tall"
html_title:           "Swift: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?

Å generere tilfeldige tall er en vanlig oppgave for programmerere. Dette innebærer å bruke et datamaskinprogram for å produsere tall som er tilfeldige eller uforutsigbare. Grunnen til at vi gjør dette er for å tilføre variasjon og usikkerhet i programmene våre, og for å løse ulike problemer som krever tilfeldighet, som spill og sikkerhetsprotokoller.

# Hvordan:

Vi kan generere tilfeldige tall enkelt ved hjelp av Swifts innebygde funksjoner. For å generere et tilfeldig tall mellom to tall, kan vi bruke ```Int.random(in: x...y)``` , hvor x og y representerer det laveste og høyeste mulige tallet i rekken. Her er en kodeblokk som illustrerer dette:

```Swift
let randomNum = Int.random(in: 1...10)
print(randomNum)
```
Dette vil produsere et tilfeldig heltall mellom 1 og 10, og skrive det ut i konsollen.

# Dykk dypere:

Historisk sett har generering av tilfeldige tall vært en kompleks og krevende oppgave. Tidlige datamaskiner brukte fysiske fenomener som radioaktivt henfall og støy for å produsere tilfeldige tall. I dag bruker vi algoritmer og pseudorandom generatorer for å simulere tilfeldighet. En alternativ måte å generere tilfeldige tall på er å bruke eksterne tjenester som tilbyr "hardware entropy", som tar i bruk fysiske verdier som innspill for å generere tilfeldige tall.

Implementasjonen av tilfeldige tall i Swift er basert på en variant av Mersenne Twister algoritmen. Denne algoritmen er utviklet for å produsere høykvalitets tilfeldige tall og er mye brukt i ulike programmeringsspråk og applikasjoner.

# Se også:

For mer informasjon om tilfeldige tall i Swift, se dokumentasjonen for ```Int.random()``` og ```Double.random()```. Du kan også sjekke ut diskusjonsforumet for å få tips og triks fra andre Swift-programmerere om å generere tilfeldige tall.