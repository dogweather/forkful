---
title:                "Refaktorisering"
date:                  2024-01-26T01:16:31.369124-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisering"

category:             "Bash"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/refactoring.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Refaktorisering är processen att omstrukturera befintlig dator kod utan att ändra dess externa beteende. Det är en vital metod för att reducera komplexitet, förbättra underhållbarheten och hålla din kodbas frisk och lättare att förstå för både nuvarande och framtida utvecklare.

## Hur gör man:
Låt oss överväga ett enkelt Bash-skript som behöver någon refaktorisering. Det är klumpigt, med upprepad kod och svårt att följa:

```Bash
#!/bin/bash
echo "Ange ett filnamn:"
read filename
if [ -f "$filename" ]; then
    echo "Filen finns."
    count=$(grep -c "foo" "$filename")
    echo "Ordet foo förekommer $count gånger."
else
    echo "Filen finns inte."
fi
```

Refaktorisering för tydlighet och återanvändbarhet kan involvera introduktion av funktioner och hantera fel mer nådigt:

```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "Ange ett filnamn:"
    read -r filename
    echo "Ange ordet att söka efter:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "Ordet $word förekommer $count gånger."
    else
        echo "Filen finns inte." >&2
        exit 1
    fi
}

main "$@"
```

Den refaktoriserade versionen använder funktioner för att förbättra läsbarheten och möjliggör potentiell återanvändning.

## Djupdykning:
Refaktorisering är inte ett koncept som ursprungligen kom med Bash eller ens högnivå programmeringsspråk; det är lika gammalt som programmering självt. Termen formaliserades i boken "Refaktorisering: Förbättring av konstruktionen av befintlig kod" av Martin Fowler år 1999, med fokus främst på objektorienterade språk.

I sammanhanget av Bash-scripting betyder refaktorisering ofta att bryta ner långa skript i funktioner, reducera upprepning med loopar eller villkor, och undvika vanliga fallgropar som att inte hantera blanksteg i filnamn. Alternativ till Bash för skript som har blivit för komplexa inkluderar Python eller Perl, som erbjuder bättre datastrukturer och felhantering för komplexa uppgifter.

Bash-specifik refaktorisering handlar mer om att hålla sig till bästa praxis, såsom att citera variabler, använda `[[ ]]` för tester över `[ ]`, och föredra `printf` över `echo` för robust utmatning. Implementationdetaljerna kretsar ofta kring att följa stilguider och att använda verktyg som `shellcheck` för statisk analys för att fånga vanliga misstag.

## Se även:
- [Googles Shell-stilguide](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, ett statiskt analysverktyg för shellskript](https://www.shellcheck.net/)
- [Konsten att använda kommandoraden](https://github.com/jlevy/the-art-of-command-line)
