---
title:    "Fish Shell: Stor bokstaving av en streng"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Å programmere kan virke skremmende for noen, men det trenger ikke å være det. Ta for eksempel det å kapitalisere en streng, altså å gjøre den første bokstaven stor. Dette kan virke som en liten og ubetydelig operasjon, men det er faktisk en viktig del av mange programmer. La oss se nærmere på hvorfor.

## Hvordan

Coding er ikke bare for eksperter og nerder, det kan være en måte å løse hverdagslige problemer på. For å kapitalisere en streng i Fish Shell, kan du bruke en kommando som heter "capitalize". Her er et eksempel på hvordan du kan bruke den:

```
Fish Shell (commandline):
$ capitalize "heisann"
Heisann
```

Som du kan se, blir den første bokstaven i strengen "heisann" automatisk gjort stor. Dette kan være nyttig for å få en mer ryddig og konsistent tekst i programmene dine. Men hva om du vil kapitalisere alle ord i en setning? Da kan du bruke kommandoen "capitalize --all".

```
Fish Shell (commandline):
$ capitalize --all "hei, jeg heter maria"
Hei, Jeg Heter Maria
```

Som du ser, blir nå alle ordene i setningen med stor forbokstav. Dette kan være nyttig for å gjøre tekst mer leselig og presentabel i brukergrensesnitt.

## Dypdykk

Å kapitalisere en streng kan virke som en enkel operasjon, men det finnes faktisk mange forskjellige måter å gjøre det på i Fish Shell. En annen måte er å bruke en tilpasset funksjon med "string" kommandoen. Her er et eksempel på hvordan det kan gjøres:

```
Fish Shell (commandline):
$ string upper -r "tekst som skal kapitaliseres"
Tekst Som Skal Kapitaliseres
```

Dette betyr at du kan lage dine egne funksjoner for å kapitalisere strenger, og tilpasse dem etter dine behov. Dette kan være nyttig hvis du vil lage en spesiell formatering for en bestemt del av teksten din.

## Se også

Her er noen relevante linker for å lære mer om programmering i Fish Shell:

- [Official Fish Shell documentation] (https://fishshell.com/docs/current/index.html)
- [Fish Shell tutorial] (https://fishshell.com/docs/current/tutorial.html)
- [10 Essential Fish Shell Commands] (https://www.hostinger.com/tutorials/fish-shell-commands)
- [How to Write a Fish Shell Script] (https://dev.to/turtle1985/how-to-write-a-fish-shell-script-4j1)