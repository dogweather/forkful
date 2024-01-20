---
title:                "Skriva till standardfel"
html_title:           "Python: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standardfel är en viktig del av programmering eftersom det låter utvecklare skriva felmeddelanden eller andra viktiga meddelanden till ett speciellt ställe istället för att blanda upp det med vanlig utmatning. Detta gör det enklare att hitta och åtgärda fel i koden.

## Hur gör man:
För att skriva till standardfel i Python, använder man funktionen `sys.stderr.write()`. Detta tar in ett meddelande som argument och skriver det till standardfel. Här är ett exempel:

```Python
import sys

sys.stderr.write("Detta är ett felmeddelande.")
```

**Output:**

`Detta är ett felmeddelande.`

## Djupdykning:
Att kunna skriva till standardfel är viktigt eftersom det ger utvecklare en enkel sätt att visa felmeddelanden utan att störa den vanliga koden. Innan standardfel, var ett sätt att logga fel till en fil, men detta var oftast krångligt och ineffektivt. Andra alternativ är att skriva till standardutmatning, men detta kan bli rörigt och svårt att hitta rätt information. 

För att skriva till standardfel i Python, använder man en inbyggd metod som kallas `sys.stderr.write()`. Denna metod tar in ett sträng-argument och skriver det till standardfel. Detta kan också kombineras med andra metoder som `sys.exit()` för att tydligt visa att det har skett ett fel och avsluta programmet.

## Se även:
- [Python dokumentation om skrivning till standardfel](https://docs.python.org/3/library/sys.html#sys.stderr)
- [En diskussion om användningen av standardfel på Stack Overflow (engelska)](https://stackoverflow.com/questions/53395323/when-to-use-sys-stdout-write-instead-of-print)