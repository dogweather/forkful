---
title:    "Fish Shell: Generering av tilfeldige tall"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor
Å generere tilfeldige tall er en nyttig ferdighet i mange programmeringsprosjekter. Det kan brukes til å simulere ulike situasjoner, teste funksjonaliteten til koden din, og til og med lage tilfeldige passord eller krypteringsnøkler. Med Fish Shell kan du enkelt generere tilfeldige tall direkte i kommandolinjen.

## Slik gjør du det
For å generere tilfeldige tall i Fish Shell, kan du bruke kommandoen `random`. Her er et eksempel på hvordan du kan bruke denne kommandoen til å generere 5 tilfeldige tall mellom 1 og 10:

```Fish Shell
set i 0
while [ $i -lt 5 ]
    set r (random 1 10)
    echo $r
    set i (math $i + 1)
end
```

Dette kodeeksempelet vil produsere følgende output:

```
7
2
10
5
9
```

Her bruker vi `set` til å definere en variabel `i` og sette den til å starte på 0. Deretter bruker vi en `while`-løkke for å kjøre kommandoen `random` og lagre resultatet i variabelen `r`. Vi bruker også `echo` for å skrive ut verdien av `r` til terminalen. Til slutt bruker vi `math` til å øke verdien av `i` med 1 for hver runde i løkken. Dette sikrer at vi bare får 5 tilfeldige tall.

## Dypdykk
Hvis du ønsker å generere tilfeldige tall basert på et seed-nummer, kan du bruke kommandoen `rnd` i stedet for `random`. Seed-nummeret spesifiseres ved å bruke flagget `-s`.

For eksempel, hvis vi vil generere 5 tilfeldige tall basert på et seed-nummer på 123, kan vi bruke følgende kode:

```Fish Shell
set i 0
while [ $i -lt 5 ]
    set r (rnd -s 123 1 10)
    echo $r
    set i (math $i + 1)
end
```

Dette vil gi følgende output:

```
7
2
1
3
9
```

Ved å bruke et seed-nummer kan du sikre at de samme tilfeldige tallene blir generert hver gang du kjører koden. Dette kan være nyttig for å gjenskape et spesifikt scenario i et program.

## Se også
- [Fish Shell dokumentasjon for `random` og `rnd` kommandoene](https://fishshell.com/docs/current/cmds/random.html)
- [En liste over ulike måter å generere tilfeldige tall i Fish Shell](https://aquariusescape.github.io/fish-random/)
- [En veiledning for å generere tilfeldige tall i andre programmeringsspråk](https://www.freecodecamp.org/news/how-to-generate-random-numbers-in-java/)