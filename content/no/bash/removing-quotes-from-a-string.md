---
title:                "Fjerne anførselstegn fra en streng"
date:                  2024-01-26T03:37:38.853047-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fjerne anførselstegn fra en streng"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å fjerne anførselstegn fra en streng innebærer å strippe bort anførselstegnene som omslutter strengen. Programmerere ønsker ofte å gjøre dette for å rense inngangsdata, forberede data for sammenligningsformål eller overholde et spesifikt dataformat når de samhandler med andre programmer eller systemer.

## Hvordan:
Bash har flere måter å fjerne anførselstegn fra strenger på. Her er noen raske eksempler:

```Bash
#!/bin/bash

# Ved bruk av variabelsubstitusjon for å fjerne både enkle og doble anførselstegn
STRING="\"Hei, Verden!\""
echo ${STRING//\"}

# Ved bruk av `tr` for å slette anførselstegn
STRING="'Hei, Verden!'"
echo $STRING | tr -d "\'"

# Ved bruk av `sed` for å slette anførselstegn
STRING="\"Hei, Verden!\""
echo $STRING | sed 's/"//g'
```

Eksempelutdata:

```
Hei, Verden!
Hei, Verden!
Hei, Verden!
```

## Dypdykk
Langt tilbake, var Unix-kommandoer som `tr` og `sed` de primære verktøyene for tekstbehandling. De er fremdeles i bruk i dag for deres fleksibilitet og kraft i håndtering av teksttransformasjoner som å fjerne anførselstegn. De er en grunnpilar i enhver shell-scripters verktøykasse.

Bash har siden utviklet seg, og variabelsubstitusjon legger til et ekstra lag av enkelhet for småskala strengmanipulasjoner. Det sparer deg for å pipe ut til eksterne binærfiler, noe som gjør skriptene dine litt mer effektive.

Mens `tr` er flott for å slette tegn, håndterer det ikke mer komplekse mønstre. `Sed`, på den andre siden, bruker regulære uttrykk, så det kan være overkill noen ganger og kan være tregere for enkle operasjoner.

Å velge mellom disse metodene avhenger av ditt spesifikke tilfelle. Hvis du trenger å strippe en variasjon av anførselstegn og du allerede er i konteksten av et Bash-skript, er bruk av variabelsubstitusjon en selvfølge for sin enkelhet. Men hvis du transformerer tekststrømmer eller flerlinjet data, er `tr` og `sed` dine go-to venner.

## Se også:
- GNU Bash-manualen, spesielt delene om Parameter Expansion og Shell Parameter Expansion: https://www.gnu.org/software/bash/manual/
- Manualen for `tr`-kommandoen: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Oversikten over `sed` stream-editor: https://www.gnu.org/software/sed/manual/sed.html
