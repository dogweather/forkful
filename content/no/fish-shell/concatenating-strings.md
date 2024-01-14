---
title:    "Fish Shell: Sammenslåing av strenger"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Hvorfor
Har du noen gang ønsket å kombinere flere tekststrenger sammen for å lage en lengre setning eller tekst? Det er akkurat det du kan gjøre ved å bruke Fish Shell sin "concat" kommando.

# Hvordan du gjør det
Her er et enkelt eksempel på hvordan du kan kombinere to strenger i Fish Shell:

```Fish Shell
set navn "Lisa"
set setning "Hei, mitt navn er "$navn"."
echo $setning
```
Output:
```Fish Shell
Hei, mitt navn er Lisa.
```

Som du kan se, bruker vi "set" kommandoen for å definere to variabler, "navn" og "setning". Deretter bruker vi dollartegn ($) for å referere til variablene i setningen vi ønsker å kombinere.

Du kan også kombinere flere strenger på en enkel måte ved å bruke "concat" kommandoen:

```Fish Shell
echo (concat "Jeg liker" "å spise " "sushi.")
```
Output:
```Fish Shell
Jeg liker å spise sushi.
```

# En dypere titt
Fish Shell sin "concat" kommando kan også ta i bruk andre argumenter, som for eksempel tall, med en enkel endring i syntaksen:

```Fish Shell
echo (concat "Jeg har" 30 "penger på konto.")
```
Output:
```Fish Shell
Jeg har 30 penger på konto.
```

Du kan også bruke "concat" for å kombinere variabler og tekst, og til og med matematiske uttrykk:

```Fish Shell
set tall 10
echo (concat "Du har" $tall "øre på bankkontoen din.")
echo (concat "Tilsammen har jeg " (math (mul $tall 3)) " kroner.")
```
Output:
```Fish Shell
Du har 10 øre på bankkontoen din.
Tilsammen har jeg 30 kroner.
```

Med litt kreativitet kan du bruke "concat" kommandoen til å lage alle slags kombinasjoner av tekst og variabler.

# Se også
Her er noen ressurser som kan være nyttige for å lære mer om concatenating strings:

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/concat.html)
- [Tutorialspoint om string concatenation i Fish Shell](https://www.tutorialspoint.com/fish_shell/fish_shell_string_concatenation.htm)
- [StackOverflow diskusjon om string concatenation i Fish Shell](https://stackoverflow.com/questions/50560259/fish-shell-concatenate-string-with-a-variable)

Takk for at du leste! Lykke til med å bruke "concat" kommandoen i Fish Shell.