---
title:    "Fish Shell: Innhenting av nåværende dato"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

I dagens teknologiske verden, er det viktigere enn noensinne å ha orden i filene og å kunne spore endringer. Enten du er en erfaren programmerer eller en nybegynner, er det alltid nyttig å ha datoen for når filene ble opprettet eller endret. Heldigvis kan du enkelt få tilgang til dagens dato ved hjelp av Fish Shell programming.

## Hvordan

Kodeeksempler og utgang innenfor "```Fish Shell ... ```" kodeblokker:

```
set dato (date +%d.%m.%Y)
echo $dato
```
Dette eksemplet vil returnere dagens dato i formatet dd.mm.åååå. Du kan også endre formatet til å inkludere klokkeslett ved å bruke kommandoen 'time' før 'date' kommandoen.

```
set dato (time -f "%d.%m.%Y %H:%M" (date))
echo $dato
```
Dette vil returnere dagens dato og klokkeslett i formatet dd.mm.åååå tt:mm.

## Dykk dypere

Det er også mulig å få tak i informasjon om spesifikke datoer ved å bruke kommandoen 'date' med '-d' flagget og angi en dato i yyyy-mm-dd format. Du kan også bruke ulike formater for å få ønsket utgang:

```
date -d "2022-01-01" +"Året er %Y"
date -d "2022-01-01" +"På denne datoen vil ingen huske korona: %A, %d/%m/%Y"
```

I tillegg kan du også bruke kommandoen 'cal' for å få en kalender for en spesifikk måned og år:

```
cal 12 2021
```

## Se også

- Fish Shell offisiell dokumentasjon: https://fishshell.com/docs/current/index.html
- Enkle måter å organisere og administrere filer ved hjelp av Fish Shell: https://medium.com/@GodelTech/how-to-manage-your-files-using-fish-shell-98b4cea6a5d6
- Fordeler ved å bruke Fish Shell for programutvikling: https://towardsdatascience.com/fish-shell-for-application-development-41a7e30e51e8