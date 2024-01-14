---
title:                "Fish Shell: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en programmerer som ønsker å feilsøke og forbedre koden din, er utskrift av debug-informasjon en uvurderlig verktøy. Det lar deg se hva som skjer på ulike punkter i koden din og identifisere eventuelle problemer. I denne blogginnlegget vil vi vise deg hvordan du kan skrive debug-utskrifter i Fish Shell.

## Slik gjør du det

For å skrive ut debug-informasjon i Fish Shell, kan du bruke kommandoen `echo` etterfulgt av `command status`. Dette vil skrive ut statusmeldinger, feilmeldinger og annen informasjon når du kjører programmer eller kommandoer. Her er et eksempel:

```Fish Shell
echo Something went wrong here!
echo command status
```

Dette vil produsere følgende utdata:

```
Something went wrong here!
0
```

Som du kan se, vil `echo`-kommandoen skrive ut teksten du har oppgitt, mens `command status` returnerer statuskoden for den siste kommandoen som ble utført. Det kan være nyttig å inkludere begge disse i dine debug-utskrifter for å få en bedre forståelse av hva som skjer i koden din.

Du kan også bruke den innebygde funksjonen `debug` i Fish Shell for å få mer detaljert informasjon om hva som skjer mens du kjører koden din. For å bruke denne funksjonen, kan du legge til `debug` foran kommandoen din. Her er et eksempel:

```Fish Shell
debug ls
```

Dette vil produsere en lang liste med informasjon om alle handlingene som utføres mens `ls`-kommandoen kjører.

## Dypdykk

Det er også mulig å skrive ut variabler og andre verdier ved hjelp av `echo`-kommandoen. For å skrive ut en variabel, kan du bruke syntaksen `$variabelnavn`. Du kan også bruke dette for å kombinere variabler og statisk tekst i en utskrift. Her er et eksempel:

```Fish Shell
set navn "Lise"
echo "Hei, mitt navn er $name!"
```

Dette vil skrive ut følgende:

```
Hei, mitt navn er Lise!
```

Hvis du ønsker å skrive ut en langs liste med verdier, kan du bruke en `for`-løkke. Dette vil gjøre det enklere å se alle verdiene og sammenligne dem. Her er et eksempel:

```Fish Shell
set tall (seq 100)
for tall in $tall
echo $tall
end
```

Dette vil skrive ut alle tallene fra 1 til 100 på hver sin linje.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/) 
- [Powerful Debugging with the Fish Shell](https://mattdmuffin.com/blog/2012/03/08/powerful-debugging-fish-shell/) 
- [Debugging Made Easy with the Fish Shell](https://hackernoon.com/debugging-made-easy-with-fish-shell-47b3me)