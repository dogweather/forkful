---
title:    "Bash: Skapa en tillfällig fil"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva Bash-skript kan vara till nytta för många olika uppgifter, inklusive att skapa och hantera temporära filer. Detta kan vara användbart för att tillfälligt lagra data, utföra beräkningar eller utföra andra operationer som inte behöver sparas permanent.

## Så här gör du

Att skapa en temporär fil i Bash är enkelt. Först använder du kommandot `mktemp` som skapar en unik fil med ett slumpmässigt namn. Detta kommando har flera olika flaggor som kan användas för att ange vilken typ av fil du vill skapa och var den ska sparas.

```Bash
# Skapar en temporär fil i aktuell mapp
tmpfile=$(mktemp)

# Skapar en temporär katalog
tmpdir=$(mktemp -d)

# Skapar en temporär fil med önskat prefix
tmpfile=$(mktemp myprefix.XXXXXX)
```

När du har skapat filen kan du använda den som vilken annan fil som helst i ditt skript. När ditt skript avslutas kommer filen eller katalogen att raderas automatiskt.

## Djupdykning

För att få ännu mer kontroll över skapandet av din temporära fil kan du använda `mktemp` tillsammans med `trap` kommandot. Genom att använda `trap` kan du definiera en funktion som kommer att köras när ditt skript avslutas, oavsett hur det avslutas. Detta är särskilt användbart om du vill säkerställa att din temporära fil alltid tas bort.

```Bash
#!/bin/bash

# Skapar en temporär fil och sätter en 'trap'
# för att ta bort filen när skriptet avslutas
tmpfile=$(mktemp)
trap "rm $tmpfile" EXIT

# Använd filen i ditt skript
echo "Det här är en temporär fil" > $tmpfile

# Avslutar skriptet
exit
```

## Se även

- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Linux Documentation Project](https://tldp.org/)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/)