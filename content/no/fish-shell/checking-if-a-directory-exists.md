---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:56:05.766314-07:00
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en mappe finnes betyr å bekrefte om en bestemt mappe er tilgjengelig på filsystemet. Programmerere gjør dette for å sørge for at scriptene deres ikke krasjer når de prøver å lese fra eller skrive til mapper som ikke eksisterer.

## Hvordan:
Inne i Fish Shell, kan du sjekke om en mappe finnes slik:

```Fish Shell
if test -d /sti/til/mappen
    echo "Mappen finnes!"
else
    echo "Mappen finnes ikke."
end
```

Her er et eksempel på output:

```Fish Shell
Mappen finnes!
```

eller

```Fish Shell
Mappen finnes ikke.
```

## Dypdykk
Å sjekke om mapper finnes er en vanlig operasjon og har vært en del av Unix-lignende shells helt siden de tidligste dagene av operativsystemer. I Fish Shell brukes `test` kommandoen - som er en bygget-inn kommando - til å utføre denne sjekken. Alternativt kan programmerere bruke `and` og `or` konstruksjoner for å kjede sammen tester, som kan bidra til mer kompakt kode når det kombineres med kondisjonell logikk. På implementeringsnivået bruker `test` systemkall for å innhente informasjon om filsystemet, som `stat` under Linux, som henter filstatus.

## Se Også
- [Fish Shell Documentation](https://fishshell.com/docs/current/)
- [Filesystem Hierarchy Standard](https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard)