---
title:    "Go: Generering av tilfeldige tall"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Hvorfor

Generering av tilfeldige tall er en viktig del av programmering, spesielt i spill og simuleringer. Det kan legge til en tilfeldig og uforutsigbar faktor som gjør programmene mer realistiske og interessante.

# Hvordan

For å generere tilfeldige tall i Go, kan vi bruke pakken "math/rand". Først må vi importere pakken ved å skrive ```import "math/rand"```, deretter må vi sette en "seed" for å sikre at tallene blir generert på en tilfeldig måte. Dette kan gjøres ved å bruke funksjonen ```rand.Seed(seed)```, der "seed" er en hvilken som helst integer verdi.

For å generere et tilfeldig tall innenfor et bestemt område, kan vi bruke ```rand.Intn(n)```, der "n" er det øvre grensen for det genererte tallet. For eksempel, for å generere et tilfeldig tall mellom 1 og 10, kan vi skrive ```rand.Intn(10)+1```.

Hvis vi ønsker å generere et tilfeldig flyttall, kan vi bruke ```rand.Float64()```, som vil gi et tall mellom 0 og 1. For å få et tall innenfor et bestemt område, kan vi multiplisere resultatet med differansen mellom de to tallene, og deretter legge til det minste tallet. For eksempel, for å få et flyttall mellom 5 og 10, kan vi skrive ```rand.Float64()*(10-5)+5```.

# Dypdykk

Det er viktig å huske at tilfeldige tall som genereres av datamaskiner ikke er helt tilfeldige, men basert på algoritmer. Derfor kan de ikke brukes til sikkerhetsformål, som for eksempel kryptering. Det er også viktig å sette en ny "seed" for hver gang vi ønsker å generere nye tilfeldige tall, ellers vil sekvensen være den samme hver gang programmet kjører.

En annen viktig ting å huske på er at hvis du skal bruke tilfeldige tall i en løkke, må du sette "seed" utenfor løkken. Dette vil sikre at tallene blir generert på en annen måte i hver iterasjon.

# Se Også

- https://www.golangprograms.com/how-to-generate-random-number.html
- https://gobyexample.com/random-numbers
- https://golang.org/pkg/math/rand/