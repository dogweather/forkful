---
title:    "Bash: Oppretting av en midlertidig fil"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Å opprette en midlertidig fil er et vanlig programmeringsteknikk som kan være nyttig når man jobber med å lagre midlertidige data eller behandle store mengder informasjon. Det kan også brukes for å sikre at programmet ditt fungerer som det skal ved å teste det med forskjellige datasett.

# Hvordan

For å opprette en midlertidig fil i Bash, kan du bruke kommandoen "mktemp". Dette vil automatisk generere en unik filnavn og opprette en tom fil med dette navnet. For eksempel:

```Bash
mktemp
```

Dette vil produsere følgende utgang:

```Bash
/tmp/tmp.x6W2VVJi
```

Du kan også spesifisere et prefiks for filnavnet ved å bruke "-p" flagget. For eksempel:

```Bash
mktemp -p myfiles
```

Dette vil produsere følgende utgang:

```Bash
myfiles/tmp.p2hNDHAY
```

Du kan også angi en filtype ved hjelp av "-t" flagget. For eksempel:

```Bash
mktemp -t .txt
```

Dette vil produsere følgende utgang:

```Bash
/tmp/tmp.JR5j9Fpp.txt
```

# Dypdykk

Når du oppretter en midlertidig fil, vil det genererte filnavnet være unikt og tilfeldig. Dette sikrer at filen ikke vil kollidere med andre filer som allerede eksisterer eller som kan bli opprettet senere. Det er også viktig å slette den midlertidige filen når den ikke lenger trengs, for å unngå rot og hindre at personlige data lekkes.

For å slette den midlertidige filen, kan du bruke "rm" kommandoen og spesifisere navnet på filen. For eksempel:

```Bash
rm /tmp/tmp.x6W2VVJi
```

En annen fordel med å bruke midlertidige filer er at de er automatiske slettes når systemet blir startet på nytt, noe som hjelper til å frigjøre plass på harddisken.

# Se Også
- [Bash dokumentasjon](https://www.gnu.org/software/bash/)
- [mktemp dokumentasjon](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)