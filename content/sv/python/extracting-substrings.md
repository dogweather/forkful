---
title:    "Python: Extrahera delsträngar"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför
Att extrahera substrängar, eller mindre delar av en större textsträng, är en avgörande färdighet för alla som programmerar i Python. Det kan användas för att manipulera och bearbeta textdata på ett effektivt sätt.

## Hur man gör det
För att extrahera en substräng i Python använder man sig av metoden `substring()` eller genom att använda indexing och slicing. Nedan följer några exempel på hur man kan använda dessa metoder:

```Python
text = "Detta är en textsträng"

# Användning av substring() för att extrahera bokstäverna "en"
substring = text.substring(11, 13)
print(substring) # en

# Användning av indexing för att extrahera de första 5 bokstäverna
substring = text[0:5]
print(substring) # Detta

# Användning av slicing för att extrahera de sista 6 bokstäverna
substring = text[17:]
print(substring) # textsträng
```

I det första exemplet använder vi oss av `substring()` metoden och anger index för de bokstäver som vi vill extrahera i parenteserna. I det andra och tredje exemplet använder vi oss av indexing respektive slicing, vilket innebär att vi anger start- och slutindex för det område av texten som vi vill extrahera.

## Deep Dive
En viktig sak att komma ihåg när man extraherar substrängar är att indexering börjar från 0 i Python, vilket innebär att det första tecknet i en sträng har indexet 0. Man kan också ange ett negativt index, vilket gör att man räknar bakifrån i strängen. Till exempel har det sista tecknet i en sträng indexet -1, det näst sista tecknet har indexet -2 och så vidare.

Det finns också olika sätt att använda slicing för att extrahera delar av en textsträng. Man kan ange en tredje parameter efter det andra indexet för att bestämma hur många tecken som ska hoppas över i varje iteration. Man kan också ange enbart det första eller andra indexet, vilket innebär att slicing kommer att börja från början respektive sluta vid slutet av strängen.

Det finns många andra olika metoder och tekniker för att extrahera substrängar i Python, men det är dessa grundläggande metoder som är de vanligaste och mest användbara.

## Se även
- [Officiell Python dokumentation om textsträngar](https://docs.python.org/2/library/string.html)
- [Tutorial om substrängar i Python](https://realpython.com/python-strings/)
- [Mer information om indexering och slicing i Python](https://www.datacamp.com/community/tutorials/python-string-slicing)