---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

# Sammanslåning av Strängar i Bash: Vad, Varför och Hur?

## Vad & Varför?
Sammanslåning av strängar betyder att man sammanfogar två eller flera strängar till en enkel sträng. Programmerare gör detta för att generera dynamiskt innehåll, konstruera kommandon, pather och andra strukturer.

## Hur man gör:
Här är några exempel på hur du kan sätta ihop strängar i Bash:

```Bash
# Exempel 1: Enkel sammansättning
sträng1="Hej, "
sträng2="världen!"
sammansattSträng=$sträng1$sträng2

echo $sammansattSträng
```

Detta kommer att skriva ut "Hej, världen!".

```Bash
# Exempel 2: Strängsammansättning i en loop
prefix="Artikel "
for ((i=1; i<=5; i++)); do
    echo $prefix$i
done
```

Denna kod kommer att skapa fem strängar - "Artikel 1" till "Artikel 5".

## Djupdykning
Sammanslåning av strängar i Bash har varit en del av språket sedan dess början på 1980-talet. Det var, och är fortfarande, kärnan i shell-programmering, och det har många användningar, från att bygga enkla meddelanden till att skapa komplexa skript.

Alternativen till strängsammansättning inkluderar användning av externa kommandon som `printf` och `awk`. De är kraftfulla, men de kan vara långsammare än native Bash strängsammansättning eftersom de kräver att Bash ska skapa en ny process.

Bash utför strängkonkatenering genom att helt enkelt skriva strängarna direkt efter varandra - ingen särskild operator krävs. Detta skiljer sig från många andra programmeringsspråk, som Python och JavaScript, som använder operatörer som `+` och `&` för att sätta ihop strängar.

## Se också
- För grundläggande Bash-strängoperationer, se [Bash string manipulations](https://tldp.org/LDP/abs/html/x24683.html).
- För mer avancerade strängoperationer med externa kommandon, se [Awk commands](https://www.grymoire.com/Unix/Awk.html) och [Printf syntax](https://www.man7.org/linux/man-pages/man1/printf.1.html).