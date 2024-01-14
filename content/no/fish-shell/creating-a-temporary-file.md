---
title:    "Fish Shell: Å opprette en midlertidig fil"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Å lage en midlertidig fil kan være nyttig i mange situasjoner når man koder i Fish Shell. Det kan hjelpe med å organisere data eller midlertidig lagre informasjon som skal brukes senere.

## Hvordan

For å lage en midlertidig fil i Fish Shell, kan du bruke `mktemp` kommandoen. Dette vil lage en unik og sikker fil i `/tmp` mappen. Du kan også angi et navn eller filtype ved å bruke `-p` og `-t` argumentene. Her er et eksempel på hvordan du kan bruke `mktemp` kommandoen:

```
Fish Shell:
mktmep tempfil

```

Dette vil lage en fil med navnet `tempfil` i `/tmp` mappen. Du kan også spesifisere en bestemt filtype ved å bruke `-t` argumentet. For eksempel:

```
Fish Shell:
mktemp -t ".txt" tempfil

```

Dette vil lage en tekstfil med navnet `tempfil` i `/tmp` mappen. Du kan også spesifisere en annen plassering for den midlertidige filen ved å bruke `-p` argumentet. For eksempel:

```
Fish Shell:
mktemp -p "~/Desktop" tempfil

```

Dette vil lage en midlertidig fil med navnet `tempfil` på skrivebordet ditt.

## Dykk dypere

For mer avansert bruk av `mktemp` kommandoen, kan du bruke `-d` argumentet for å lage en midlertidig mappe i stedet for en fil. Dette er nyttig når du trenger å lagre flere filer midlertidig og organisere dem i en mappe. For eksempel:

```
Fish Shell:
mktemp -d tempmappe

```

Dette vil lage en mappe med navnet `tempmappe` i `/tmp` mappen. Du kan også bruke `--tmpdir` argumentet for å spesifisere en annen plassering for den midlertidige mappen.

En annen nyttig funksjon ved å bruke `mktemp` er muligheten til å lage flere midlertidige filer eller mapper på en gang. Du kan bruke `*` som et jokertegn for å lage flere filer eller mapper med lignende navn. For eksempel:

```
Fish Shell:
mktemp tempfil*

```

Dette vil lage to midlertidige filer med navnene `tempfil1` og `tempfil2` i `/tmp` mappen.

## Se også

- [Fish Shell dokumentasjon for mktemp kommandoen](https://fishshell.com/docs/current/commands.html#mktemp)
- [Linux Dokumentasjon Project om temporary files](https://www.tldp.org/LDP/Linux-Filesystem-Hierarchy/html/tmp.html)
- [Eksempelkoder og bruk av mktemp kommandoen i Fish Shell](https://github.com/jorgebarron/FishShell-TemporaryFiles)