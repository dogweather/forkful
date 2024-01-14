---
title:                "Bash: Å skrive til standardfeil"
simple_title:         "Å skrive til standardfeil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standardfeil i Bash-programmering kan være en nyttig måte å håndtere feil og informasjon på. Ved å sende ut feilmeldinger og annen informasjon til standardfeilen, kan du enkelt fange og håndtere disse dataene i ditt program.

## Slik gjør du det

For å skrive til standardfeil i Bash kan du bruke kommandoen `2>&1`. Dette betyr at du sender det som normalt ville gått til standard feil (2) til standard ut (1). Her er et eksempel:

```Bash
#!/bin/bash
echo “Denne teksten vil bli sendt til standardfeil” 1>&2
```
Dette vil sende teksten "Denne teksten vil bli sendt til standardfeil" til standardfeilen i stedet for standarduttaket. Merk at tallet for standard feil er 2, mens standard ut er 1.

Du kan også bruke `>&` for å kombinere standard feil og standard ut i en linje. For eksempel:

```Bash
#!/bin/bash
echo “Denne teksten vil bli sendt til både standardfeil og standard ut” >&2
```

Hvis du vil sende en feilmelding til standardfeil, kan du bruke `2>/dev/null`. Dette vil sende feilmeldingen til standardfeilen, men samtidig ignorere den ved hjelp av `/dev/null`-kommandoen.

## Dykk dypere

Når du skriver til standardfeil, er det viktig å merke seg at dataene som blir sendt dit er non-buffered. Dette betyr at de blir sendt umiddelbart til standardfeilen, i motsetning til standard ut som kan lagres i en buffer og sendes senere. Det kan lønne seg å bruke `tee`-kommandoen for å lagre dataene til en fil hvis du ønsker å beholde dem for senere bruk.

Det er også viktig å håndtere feil riktig når du sender dem til standardfeil. Sørg for at du bruker riktig exit-kode og gir tydelige og informative feilmeldinger for å hjelpe deg med å feilsøke programmet ditt.

## Se også

- [Bash scripting tutorial på Devhints](https://devhints.io/bash)
- [Offisiell Bash dokumentasjon](https://www.gnu.org/software/bash/manual/bash.html)
- [Kjøring av Bash-kommandoer fra standard ut](https://wiki.bash-hackers.org/howto/redirection_tutorial)