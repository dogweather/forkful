---
title:    "Bash: Läsa kommando rad argument"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför

Många program, speciellt skript, behöver kunna ta emot olika argument från användaren för att kunna fungera på olika sätt. Genom att lära sig hur man läser in kommandoradsargument kan du skapa mer robusta och anpassningsbara skript.

## Hur man läser in kommandoradsargument

För att läsa in kommandoradsargument i Bash använder man variabeln $1, som representerar det första argumentet som matas in av användaren. $2, $3 och så vidare används för respektive tillhörande argument. Nedan följer ett enkelt exempel där vi läser in två argument och sedan skriver ut dem i terminalen:

```Bash
echo "Det första kommandoradsargumentet är $1"
echo "Det andra kommandoradsargumentet är $2"
```

Om vi kör detta skript med kommandot "bash minskript.sh hej hejdå" kommer terminalen att visa:

```Bash
Det första kommandoradsargumentet är hej
Det andra kommandoradsargumentet är hejdå
```

Om du vill läsa in ett okänt antal kommandoradsargument kan du använda en loop och variabeln $# som representerar det totala antalet argument som matats in av användaren. Till exempel:

```Bash
for ((i=1; i<=$#; i++)); do
   argument=$i
   echo "Argument $i: ${!argument}"
done
```

Detta kommer att skriva ut alla kommandoradsargument som matas in, oberoende av hur många det är.

## Djupdykning

Det finns flera sätt att läsa in kommandoradsargument på, beroende på vad du vill göra med dem. Du kan till exempel använda ett switch-case statement för att utföra olika åtgärder baserat på vilket argument som matas in. Du kan även använda flaggor för att ange specifika alternativ när du kör skriptet, till exempel "-a" för att göra en viss åtgärd och "-b" för en annan.

Det är också viktigt att hantera felaktiga eller otillräckliga argument som kan skapas av användaren. Ett sätt att göra detta är genom att använda en if-sats och kontrollera antalet arguments som matas in. Om du till exempel behöver minst två argument för att ditt skript ska fungera korrekt, kan du göra följande:

```Bash
if [[ $# -lt 2 ]]; then
    echo "Du behöver ange minst två kommandoradsargument"
fi
```

Genom att hantera kommandoradsargument på ett korrekt sätt kan du skapa mer funktionella och användbara skript.

## Se också

- [Kommandoradsargument i Bash](https://www.geeksforgeeks.org/command-line-arguments-in-bash-scripting/)
- [Looping genom kommandoradsargument](https://linuxize.com/post/bash-for-loop/)
- [Hantera felaktiga kommandoradsargument](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_02.html)