---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Extrahering av understrängar i Bash innebär att plocka ut specifika segment från en given sträng. Programmerare gör det för att bearbeta eller analysera data effektivt.

## Hur till:
Nedan är kodexemplar och därmed utdata med `Bash`. Här ska vi extrahera "pro" från strängen "programmering".

```Bash
STR='programmering'
SUBSTR=${STR:0:3}
echo $SUBSTR
```
Utfallet blir "pro".

## Djupdykning
Bash, födelseår 1989 av Brian Fox, var en gratis ersättning till det då populära Bourne-shellskriptet. Extrahering av understrängar introducerades först i version 2.0.

Alternativen till Bash innefattar till exempel sh, zsh och PowerShell. Alla dessa har olika sätt att hantera extrahering av understrängar.

En historisk notering är att Bash inte hade stöd för understrängsmanipulation innan version 2. Denna funktion implementerades för att effektivisera strängbearbetningen, en av de mest grundläggande operationerna i skript och programmering.

## Se också
Här är några länkar till relaterade resurser:

1. [Bash manual: Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion) - Detaljerad dokumentation om Bash-parameterexpansion, inklusive extraktion av understrängar.
2. [Advanced Bash-Scripting Guide: Manipulating Strings](https://tldp.org/LDP/abs/html/string-manipulation.html) - Utforska mer om strängmanipulationer med Bash.
3. [Stack Overflow: How to do string extraction in Bash](https://stackoverflow.com/questions/19482123/extract-part-of-a-string-in-bash) - Diskussionsforum med exempel och lösningar för strängutdrag i Bash.