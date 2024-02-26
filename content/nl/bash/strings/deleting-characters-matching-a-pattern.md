---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:32.528387-07:00
description: "Karakters verwijderen die overeenkomen met een patroon in Bash laat\
  \ je toe om strings te manipuleren naar je behoefte\u2014zoals ongewenste tekens\
  \ strippen of\u2026"
lastmod: '2024-02-25T18:49:48.302589-07:00'
model: gpt-4-0125-preview
summary: "Karakters verwijderen die overeenkomen met een patroon in Bash laat je toe\
  \ om strings te manipuleren naar je behoefte\u2014zoals ongewenste tekens strippen\
  \ of\u2026"
title: Karakters verwijderen die overeenkomen met een patroon
---

{{< edit_this_page >}}

## Wat & Waarom?
Karakters verwijderen die overeenkomen met een patroon in Bash laat je toe om strings te manipuleren naar je behoefte—zoals ongewenste tekens strippen of input sanitizen. Het is cruciaal voor het opschonen van data en het voorbereiden ervan voor verwerking.

## Hoe te:
### Leidende/afsluitende witruimte verwijderen:
```Bash
tekst="   Hallo, Wereld!   "
getrimd=$(echo "$tekst" | xargs)
echo "$getrimd"
```
Output: `Hallo, Wereld!`

### Alle cijfers verwijderen:
```Bash
tekst="B4sh i5 geweldig!"
opgeschoond=${tekst//[^a-zA-Z ]/}
echo "$opgeschoond"
```
Output: `Bsh i geweldig`

### Specifieke karakters vervangen:
```Bash
tekst="Hallo-Wereld!"
opgeschoond=${tekst//-/_}
echo "$opgeschoond"
```
Output: `Hallo_Wereld!`

## Diepere Duik
In het begin waren tekstverwerkingshulpmiddelen zoals `sed` en `awk` de standaard voor stringmanipulatie. Sindsdien heeft Bash patroonherkenning en stringmanipulatie rechtstreeks in de shell zelf ingebouwd, waardoor zijn gebruikers veel kracht krijgen zonder de noodzaak voor externe commando's.

De `${parameter/patroon/tekenreeks}` syntaxis is één benadering waarbij je de eerste overeenkomst van `patroon` vervangt met `tekenreeks`. Om alle overeenkomsten te verwijderen, voeg nog een `/` toe zoals getoond in de bovenstaande voorbeelden.

Alternatieven zijn het gebruiken van klassieke UNIX-hulpmiddelen zoals `sed`, `awk`, `tr`, of meer moderne scripttalen zoals Python of Perl.

Onder de motorkap gebruikt Bash globbing en wildcards voor patroonherkenning, maar wanneer je die `${tekst//patroon/}` constructies ziet, heb je te maken met de parameteruitbreiding van Bash—een functie die uiterst handig is voor stringmanipulatie.

## Zie Ook
- Bash Handleiding over Parameteruitbreiding: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Een artikel over tekstverwerking in Linux: https://www.linuxjournal.com/content/pattern-matching-bash
- Sed & Awk 101 Hacks eBook: https://www.thegeekstuff.com/ebooks/sed_awk_101_hacks
