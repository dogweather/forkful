---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konkatenere strenger betyr å slå sammen to eller flere tekststrenger til én. Denne teknikken brukes ofte av programmerere for å lage mer komplekse meldinger, generere filnavn, eller kode effektivt.

## Hvordan gjør man det:

La oss se på noen eksempler i Bash:

```Bash
# Definer to strenger
streng1="God "
streng2="morgen"

# Konkatenér strengene
hilsen=$streng1$streng2

# Skriv ut resultatet
echo $hilsen

# Utskrift blir: "God morgen"
```

Du kan også konkatenere strenger direkte i shell-kommandoer:

```Bash
navn="Ola"
echo "Hei $navn"   # Utskrift blir: "Hei Ola"
```

## Dypdykk:

Historisk sett har strengkonkatenering vært en funksjon i nesten alle programmeringsspråk, inkludert de tidlige versjonene av Bash.

En alternativ tilnærming til strengkonkatenering i Bash kan være bruken av `printf` funksjonen:

```Bash
printf -v hilsen "%s%s" $streng1 $streng2
```

Det er viktig å merke seg at i Bash, når du konkatenøser to variable sammen, slår Bash dem bare sammen uten mellomrom eller annen atskillelse. Hvis du trenger en atskillelse, må du legge den inn som en del av strengen:

```Bash
streng1="God"
streng2="morgen"
hilsen="$streng1 $streng2"   # Merk mellomromet mellom anførselstegnene
```

## Se også:

For mer informasjon om strengmanipulering i Bash, inkludert strengkonkatenering og andre teknikker, kan disse nettstedene være nyttige:

1. Advanced Bash-Scripting Guide - String Manipulation: https://tldp.org/LDP/abs/html/string-manipulation.html
2. Bash Guide - More on Variables: https://guide.bash.academy/words/#variables
3. GNU Bash Manual: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion