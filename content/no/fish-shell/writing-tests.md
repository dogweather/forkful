---
title:    "Fish Shell: Å skrive tester"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Hvorfor

Hvis du er en erfaren programmerer, så vet du at det å skrive kode bare er en del av jobben. Det å sikre at koden fungerer som den skal, er like viktig. Det er her testing kommer inn i bildet. Ved å skrive tester for din Fish Shell kode, kan du være trygg på at den vil fungere som forventet og unngå potensielle feil.

# Hvordan

For å skrive tester i Fish Shell, kan du bruke kommandoen `test`. Dette lar deg evaluere ulike uttrykk og gi en tilsvarende boolean-verdi som output.

````Fish Shell
test 1 -eq 1 # Output: True
test "hello" = "world" # Output: False
````

Du kan også bruke `not` for å invertere en boolean-verdi, og bruke parenteser for å gruppere uttrykk.

````Fish Shell
test (1 -eq 1) and (2 -gt 1) # Output: True
not test (1 -eq 1) # Output: False
````

Ved å bruke disse enkle eksemplene som utgangspunkt, kan du bygge videre og skrive mer avanserte tester for din Fish Shell kode.

# Dypdykk

Nå som du har lært det grunnleggende, kan du gå videre til å utforske flere muligheter innenfor testing i Fish Shell. Du kan bruke kommandoen `test -s` for å sjekke om en fil eksisterer, og `test -z` for å sjekke om en variabel er tom. Ved å bruke disse verktøyene vil du kunne skrive tester for en rekke ulike situasjoner.

I tillegg kan du også bruke `set -x` for å dekke debugging i dine tester. Dette vil vise deg detaljert informasjon om hva som skjer i hvert trinn av din test, og gjøre det enklere å finne potensielle feil.

# Se Også

- [Fish Shell dokumentasjon for testing](https://fishshell.com/docs/current/cmds/test.html)
- [10 grunner til å skrive tester for din kode](https://dev.to/deepk/10-reasons-why-you-should-write-tests-for-your-code-3ipk)
- [Hvordan unngå vanlige feil ved å skrive tester](https://medium.com/better-programming/how-to-avoid-common-testing-mistakes-57dc18ba56d1)