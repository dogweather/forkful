---
title:                "Skriving av en tekstfil"
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive en tekstfil betyr å lagre data i en fil som kan leses som tekst. Programmerere gjør dette for å lagre konfigurasjoner, logge informasjon eller kommunisere mellom prosesser.

## Slik gjør du:
For å skrive tekst til en fil i Fish, bruk `echo` eller `printf` sammen med omdirigering.

```Fish Shell
echo "Hei, verden!" > minfil.txt  # Skriver ny fil
echo "Husk denne linjen også" >> minfil.txt  # Legger til tekst
```

For å se innholdet i filen, bruk `cat`:

```Fish Shell
cat minfil.txt
```

Sample output:
```
Hei, verden!
Husk denne linjen også
```

## Dybde:
Historisk sett, er skriving til fil en grunnleggende funksjon i operativsystemer, ettersom de er designet for data lagring og manipulering. Alternativer i Fish Shell inkluderer bruk av `tee` for både visning og skriving samtidig, eller skript som bruker lavnivå-filsystemfunksjoner for finere kontroll. Filmanipulering i Fish er direkte og forenklet, uten behov for ekstra syntaks som i andre skall.

## Se også:
- Fish Shell dokumentasjon på filredigering: https://fishshell.com/docs/current/index.html#redirection
- Unix `tee` kommandoen: https://linux.die.net/man/1/tee
- Fish skripting tutorial: https://fishshell.com/docs/current/tutorial.html