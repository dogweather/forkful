---
title:                "Lesing av en tekstfil"
html_title:           "Fish Shell: Lesing av en tekstfil"
simple_title:         "Lesing av en tekstfil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

Hva & hvorfor?

Lesing av tekstfiler er en vanlig oppgave for programmører. Det innebærer å åpne en eksisterende tekstfil og lese informasjonen som er lagret i den. Dette er nyttig når man for eksempel trenger å analysere data eller hente informasjon fra en tekstfil.

Hvordan:

Fish Shell tilbyr en enkel og effektiv måte å lese tekstfiler på. For å lese en tekstfil ved hjelp av Fish Shell, bruk følgende kommando: 
```
cat filnavn.txt 
```
Dette vil vise innholdet i filen direkte i terminalen. For å lagre innholdet i en variabel og bruke det senere i koden, kan du bruke følgende kommando:
```
set innholdet (cat filnavn.txt)
```
Du kan deretter bruke variabelen "innholdet" til å manipulere data og utføre de ønskede operasjonene.

Deep Dive:

Å lese tekstfiler har vært en grunnleggende oppgave for programmerere siden de tidlige dagene av datamaskiner. I stedet for å skrive inn data manuelt i koden, var det mye mer effektivt å lese data fra en fil. Alternativer til Fish Shell for å lese tekstfiler inkluderer bash, zsh og PowerShell.

En interessant funksjon i Fish Shell er muligheten til å pipe flere filer sammen for å lese informasjonen fra dem samtidig. Dette er nyttig når man for eksempel trenger å sammenligne data fra to forskjellige filer. For å gjøre dette kan du bruke kommandoen:
```
cat fil1.txt fil2.txt | sort
```
Dette vil kombinere innholdet i de to filene og deretter sortere resultatet alfabetisk.

Se også:

Hvis du vil lære mer om hvordan du bruker Fish Shell til å lese tekstfiler og andre nyttige programmeringstips, kan du besøke følgende ressurser:

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current)
- [Learning Fish Shell: The Interactive Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [20 fish shell commands, aliases and configurations examples](https://www.cyberciti.biz/faq/20-fish-shell-commands-aliases-configurations/)