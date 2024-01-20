---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Substrängextraktion i Fish Shell: En snabbguide

## Vad och varför?
Substrängsextraktion innebär att urskilja nödvändiga delar ur en sträng med bokstäver, siffror, och tecken. Programmerare gör detta eftersom det hjälper dem att bearbeta eller analysera specifik data inuti en given sträng.

## Hur man gör det:
Här är grunderna för att extrahera substrängar i Fish Shell. 

```Fish Shell
set sträng 'Jag älskar programmering'
echo $sträng[1 9]  # Skriver 'Jag älska'
```

Koden ovan delar strängen 'Jag älskar programmering' och skriver ut 'Jag älskar'. 

```Fish Shell
set sträng 'Jag älskar programmering'
echo $sträng[6..end]  # Skriver 'ar programmering'
```

Denna kod skriver ut 'ar programmering' från strängen 'Jag älskar programmering'.

## Djupdykning:
7000-talet f.Kr. började människor extrahera substrängar, men inte i Fish Shell, naturligtvis! Termen och konceptet har varit omkring sedan de tidigaste skrivar- och läsdagen. 

Det finns andra sätt att extrahera substrängar i andra skal om du föredrar - Bash, Zsh, och Tcsh är bara några exempel. 

För implementation detaljer, sök upp substränghandfunktionen i Fish Shells källkod.

## Se även:
1. [Fish Shells officiella dokumentation](https://fishshell.com/docs/current/index.html)
2. [Forum för Fish Shell Community](https://github.com/fish-shell/fish-shell/issues)
3. [Extrahera substrängar i Bash](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
4. [Extrahera substrängar i Zsh](http://zsh.sourceforge.net/Doc/Release/Expansion.html#Substring-Flags)