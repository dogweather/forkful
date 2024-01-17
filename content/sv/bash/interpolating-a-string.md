---
title:                "Att interpolera en sträng"
html_title:           "Bash: Att interpolera en sträng"
simple_title:         "Att interpolera en sträng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att interpolera en sträng innebär att sätta in variabler eller kommandon i en sträng. Det används för att göra kod mer dynamisk och läsbar.

Vanligtvis använder vi detta för att skapa mer effektiva och flexibla skript där vi kan återanvända samma kod med olika värden för variablerna.

## Hur du gör:

För att interpolera en sträng i Bash, använd "echo" kommandot och sätt variabeln inuti ett dollartecken följt av ett kapiteltecken. Som exempelvis: 
```Bash
first_name="Lisa"
last_name="Andersson"
echo "Hej, mitt namn är $first_name $last_name."
```

Detta skulle ge utdatan "Hej, mitt namn är Lisa Andersson."

Om du vill använda ett kommando som en variabel, kan du använda "$($command)" notation. Till exempel: 
```Bash
current_date=$(date +%Y-%m-%d)
echo "Idag är det $current_date."
```

Detta skulle ge utdatan "Idag är det 2021-10-05."

## Fördjupning:

Historiskt sett har interpolering av strängar funnits i många programmeringsspråk sedan början av 70-talet. Det finns också andra sätt att interpolera strängar i Bash, såsom "printf" kommandot eller att använda dubbela citattecken istället för enkla för att få med variabler.

Implementeringen av interpolering i Bash är snabbare och mer effektiv än att använda "concatenation" (sammanfogning) för att skapa en sträng med variabler, eftersom den undviker att skapa onödiga kloner av strängar.

## Se även:

- [Bash Reference Manual: Command Substitution](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Command-Substitution)
- [Bash Hackers Wiki: Command substitution](https://wiki.bash-hackers.org/syntax/expansion/cmdsubst)