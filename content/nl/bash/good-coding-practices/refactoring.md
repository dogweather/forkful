---
aliases:
- /nl/bash/refactoring/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:18.483098-07:00
description: "Refactoring is het proces van het herstructureren van bestaande computercode\
  \ zonder het externe gedrag ervan te veranderen. Het is een essenti\xEBle praktijk\u2026"
lastmod: 2024-02-18 23:09:02.050052
model: gpt-4-0125-preview
summary: "Refactoring is het proces van het herstructureren van bestaande computercode\
  \ zonder het externe gedrag ervan te veranderen. Het is een essenti\xEBle praktijk\u2026"
title: Refactoring
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het herstructureren van bestaande computercode zonder het externe gedrag ervan te veranderen. Het is een essentiële praktijk om complexiteit te verminderen, onderhoudbaarheid te verbeteren en je codebasis gezond en makkelijker te begrijpen te maken voor zowel huidige als toekomstige ontwikkelaars.

## Hoe te:
Laten we een eenvoudig Bash-script overwegen dat wat refactoring nodig heeft. Het is onhandig, met herhaalde code en het is moeilijk te volgen:

```Bash
#!/bin/bash
echo "Voer een bestandsnaam in:"
read filename
if [ -f "$filename" ]; then
    echo "Bestand bestaat."
    count=$(grep -c "foo" "$filename")
    echo "Het woord foo komt $count keer voor."
else
    echo "Bestand bestaat niet."
fi
```

Refactoring voor duidelijkheid en herbruikbaarheid kan betrekking hebben op het introduceren van functies en het sierlijker afhandelen van fouten:

```Bash
#!/bin/bash

function bestaat_bestand() {
    [[ -f "$1" ]]
}

function tel_voorkomens() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "Voer een bestandsnaam in:"
    read -r filename
    echo "Voer het woord in om naar te zoeken:"
    read -r word

    if bestaat_bestand "$filename"; then
        count=$(tel_voorkomens "$word" "$filename")
        echo "Het woord $word komt $count keer voor."
    else
        echo "Bestand bestaat niet." >&2
        exit 1
    fi
}

main "$@"
```

De gerefactoreerde versie gebruikt functies om de leesbaarheid te verbeteren en maakt potentiële hergebruik mogelijk.

## Diepe Duik:
Refactoring is geen concept dat is ontstaan met Bash of zelfs met high-level programmeertalen; het is zo oud als programmeren zelf. De term werd geformaliseerd in het boek "Refactoring: Improving the Design of Existing Code" door Martin Fowler in 1999, waarbij de focus voornamelijk lag op objectgeoriënteerde talen.

In de context van Bash-scripting betekent refactoring vaak het opsplitsen van lange scripts in functies, het verminderen van herhaling met lussen of conditionals en het vermijden van veelvoorkomende valkuilen, zoals het niet correct afhandelen van spaties in bestandsnamen. Alternatieven voor Bash voor scripts die te complex zijn geworden, omvatten Python of Perl, die betere datastructuren en foutafhandeling bieden voor complexe taken.

Bash-specifieke refactoring gaat meer over het naleven van beste praktijken, zoals het citeren van variabelen, het gebruiken van `[[ ]]` voor tests in plaats van `[ ]`, en de voorkeur geven aan `printf` boven `echo` voor robuuste output. Implementatiedetails draaien vaak om het volgen van de stijlgidsen en het gebruik van tools zoals `shellcheck` voor statische analyse om veelvoorkomende fouten op te vangen.

## Zie Ook:
- [Google's Shell Stijlgids](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, een statische analysetool voor shellscripts](https://www.shellcheck.net/)
- [De Kunst van de Command Line](https://github.com/jlevy/the-art-of-command-line)
