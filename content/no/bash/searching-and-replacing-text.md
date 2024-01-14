---
title:                "Bash: Søke og erstatte tekst"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hvorfor
Å søke og erstatte tekst er en viktig del av Bash programmering. Det spiller en avgjørende rolle for å effektivisere og automatisere oppgaver, spesielt når man arbeider med store mengder data og filer.

# Hvordan
Det finnes flere måter å søke og erstatte tekst på i Bash. Den enkleste måten er å bruke kommandoen `sed` som står for "stream editor". Dette er en kraftig kommando som lar deg søke og erstatte tekst i filer uten å endre originalfilen. 

For å bruke `sed` kommandoen, må du først spesifisere tekststrengen du vil søke etter, og deretter tekststrengen du vil erstatte den med. For eksempel, hvis du vil endre alle forekomster av "Bonjour" til "Hei" i en tekstfil, kan du bruke følgende kommando:

```Bash
sed -i 's/Bonjour/Hei/g' tekstfil.txt
```

La oss ta en nærmere titt på denne kommandoen. `sed` kommandoen bruker flagget `-i` for å indikere at du vil gjøre endringer direkte i filen. Deretter bruker vi `'s/Bonjour/Hei/g'` for å spesifisere hva vi vil søke etter og erstatte med. `s` står for "substitute" (erstatt), `/Bonjour/` er tekststrengen vi vil søke etter, `/Hei/` er tekststrengen vi vil erstatte den med, og `g` betyr "global" og angir at alle forekomster skal erstattes. Til slutt indikerer vi hvilken fil dette skal utføres på, i dette tilfellet er det `tekstfil.txt`.

Du kan også søke etter og erstatte tekst i flere filer samtidig ved å spesifisere filene seperert med mellomrom, for eksempel `sed -i 's/Bonjour/Hei/g' tekstfil1.txt tekstfil2.txt`.

# Dypdykk
I tillegg til `sed` kommandoen, er det også andre verktøy som kan brukes til å søke og erstatte tekst i Bash, som for eksempel `awk` og `perl`. Disse verktøyene bruker lignende syntaks, så kunnskap om hvordan man bruker `sed` vil være nyttig når du arbeider med disse verktøyene.

Det er også mulig å bruke regulære uttrykk i søk og erstatte operasjoner for å gjøre det enda mer kraftig. Regulære uttrykk lar deg søke etter tekstmønstre i stedet for spesifikke ord. For eksempel, hvis du vil erstatte alle tall i en fil med ordet "nummer", kan du bruke følgende kommando:

```Bash
sed -i 's/[0-9]/nummer/g' tekstfil.txt
```

Her bruker vi firkantede klammeparenteser og et uttrykk for å matche alle tall (0-9). Dette vil erstatte alle tall med ordet "nummer". Mer avanserte regulære uttrykk kan også brukes for å gjøre mer komplekse søk og erstatte operasjoner.

# Se også
- [Bash sed command](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/) (artikkel på engelsk)
- [Introduction to regular expressions in Bash](https://www.linuxjournal.com/content/bash-regular-expressions) (artikkel på engelsk)
- [Bash scripting tutorials på YouTube](https://www.youtube.com/playlist?list=PLf-PXVd8LLvjIzULMtVoj9OqQ8XxichST) (videoer på engelsk)