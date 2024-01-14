---
title:                "Bash: Att omvandla en textsträng till versalisering"
simple_title:         "Att omvandla en textsträng till versalisering"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera en sträng till versaler (stora bokstäver) är en viktig teknik inom Bash programmering. Det kan användas för att förbättra utdata, filtrera data eller för att göra det enklare att jämföra strängar. Här är hur du kan göra det.

## Hur man

För att göra detta i Bash, behöver vi först definiera en variabel med en sträng och sedan applicera en inbyggd funktion för att konvertera den till versaler. Här är ett exempel på kod som visar detta i åtgärd: 

```Bash
sträng="hej, världen!"
echo ${sträng^^}
```

Det första steget är att definiera variabeln "sträng" med vår önskade text, "hej, världen!". Sedan använder vi operatorn "^^" för att konvertera hela strängen till versaler. Utan att behöva förklara logiken bakom det, kommer utdata att vara "HEJ, VÄRLDEN!". 

Ett annat sätt att konvertera är att använda key viable "tr" och dess byte funktion. Exempel:

```Bash
sträng="hej, världen!"
echo $sträng | tr '[a-z]' '[A-Z]'
```

Eftersom "tr" byte funktion byter ut alla små bokstäver till stora, måste vi bara ange vilka intervall av bokstäver som behöver ändras. I vårt exempel anger vi '[a-z]' och '[A-Z]' för att byta ut alla små bokstäver till stora bokstäver. 

## Djupdykning

Nu när vi har förstått hur man konverterar en hel sträng till versaler, låt oss ta en djupare titt på hur detta fungerar bakom kulisserna. Bash har ett inbyggt set av funktioner för behandling av strängar, från att hitta och ersätta till att konvertera versaler. Att byta ut en sträng till versaler är en del av detta set och kallas "case modification". 

Vi kan också konvertera första bokstaven i varje ord till versaler genom att använda "^^" operatorn tillsammans med "capitalize" funktionen. Exempel: 

```Bash
sträng="hej, världen!"
echo ${sträng^}
```

I detta exempel använder vi "^^" för att byta ut den första bokstaven till versaler, vilket resulterar i "Hej, världen!". Det är också möjligt att använda "^^" tillsammans med "capitalize" funktionen för att konvertera alla ord i en sträng till versaler. Exempel: 

```Bash
sträng="hej, världen!"
echo ${sträng^^*}
```

Denna gång använder vi "^^*" för att konvertera alla ord till versaler, vilket ger oss utdata "HEJ, VÄRLDEN!".

## Se även

Vill du lära dig mer om Bash programmering och dess funktioner för strängbehandling? Kolla in dessa resurser för att fördjupa din kunskap: 

- [Officiell Bash dokumentation](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Bash Guide for Beginners (engelska)](https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)
- [Bash Scripting Tutorial (engelska)](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Bash Hackers Wiki (engelska)](https://wiki.bash-hackers.org/start)
- [Bash Tips & Tricks (engelska)](https://www.shell-tips.com/bash/)