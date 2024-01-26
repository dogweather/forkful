---
title:                "Sammanslagning av strängar"
date:                  2024-01-20T17:34:20.243931-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammanslagning av strängar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? 
Sammanfogning av strängar handlar om att smälla ihop textbitar till en enda längre text. Programmerare gör det för att bygga upp meddelanden, kommandon eller för att hantera data dynamiskt.

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
