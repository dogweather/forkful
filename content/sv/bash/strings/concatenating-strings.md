---
date: 2024-01-20 17:34:20.243931-07:00
description: "How to: S\xE5 h\xE4r sm\xE4ller du ihop str\xE4ngar i Bash."
lastmod: '2024-03-13T22:44:38.072580-06:00'
model: gpt-4-1106-preview
summary: "S\xE5 h\xE4r sm\xE4ller du ihop str\xE4ngar i Bash."
title: "Sammanslagning av str\xE4ngar"
weight: 3
---

## How to:
Så här smäller du ihop strängar i Bash:

```Bash
# Direkt sammanfogning
hello="Hej, "
world="världen!"
combined=$hello$world
echo $combined
```

Output:
```
Hej, världen!
```

Med variabler, utan att skapa en ny:

```Bash
first_name="Lars"
greeting="Hej, $first_name!"
echo $greeting
```

Output:
```
Hej, Lars!
```

Om du vill lägga till en sträng till en befintlig variabel:

```Bash
prefix="I dag är det "
suffix="en bra dag."
prefix+=$suffix
echo $prefix
```

Output:
```
I dag är det en bra dag.
```

## Deep Dive
I tidiga versioner av shellskript användes externa verktyg som `expr` för att hantera strängar. Med Bash introducerades inbyggda funktioner, vilket var både snabbare och enklare. 

Alternativ till direkt sammanfogning inkluderar att använda `printf` för formatering:

```Bash
printf -v full_greeting "%s %s" "$hello" "$world"
echo $full_greeting
```

Detaljer kring sammanfogning är ganska raka i Bash, inga speciella funktioner eller operatorer krävs förutom `+` när vi adderar till en befintlig variabel.

## See Also
För vidare läsning och relaterade resurser:
- Bash Manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Bash String Manipulation Examples: https://linuxize.com/post/bash-concatenate-strings/
