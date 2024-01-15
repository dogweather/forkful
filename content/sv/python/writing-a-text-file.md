---
title:                "Att skriva en textfil"
html_title:           "Python: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Det finns många användbara anledningar till att skriva en textfil med hjälp av Python. Det kan vara för att spara data, automatiskt generera rapporter eller för att bearbeta stora mängder text. Oavsett anledning kan detta vara en användbar funktion att ha i din programmeringsverktygslåda.

## Hur man gör det

Att skriva en textfil med Python är ganska enkelt. Först måste vi öppna en ny fil som vi kommer att använda för att spara vår text. Detta görs med hjälp av "open()" funktionen och vi måste specificera filnamnet och vilket läge vi vill öppna den i.

```Python
file = open("textfil.txt", "w") # "w" står för "skrivläge"
```

Nu kan vi börja skriva vår text i filen. Vi använder "write()" funktionen för att lägga till innehåll i filen. Se till att lägga till en radbrytning efter varje rad, annars kommer all text att läggas i samma rad i filen.

```Python
file.write("Hej! Detta är min första textfil.\n")
file.write("Jag är ett stort fan av Python och använde det för att skriva denna fil.")
```

När vi är klara med att skriva vår text måste vi stänga filen för att säkerställa att allt innehåll sparas. Detta görs med "close()" funktionen.

```Python
file.close()
```

Om vi nu öppnar vår fil kommer vi att se vår text i den.

```
Hej! Detta är min första textfil.
Jag är ett stort fan av Python och använde det för att skriva denna fil.
```

## Djupdykning

Nu när vi har en grundläggande förståelse för hur man skriver en textfil i Python kan vi titta på några mer avancerade funktioner. En sak att komma ihåg är att om filen vi försöker öppna inte redan finns kommer den att skapas automatiskt. Om filen redan finns kommer allt innehåll i den att skrivas över när vi använder "w" läget.

Vi kan också använda "a" för att öppna filen i "append" läget, vilket innebär att allt nytt innehåll kommer att läggas till slutet av filen istället för att skriva över det som redan finns.

```Python
file = open("textfil.txt", "a") # "a" står för "appendläge"
```

Vi kan också använda "with" statement för att säkerställa att filen stängs automatiskt när vi är klara med den, även om det uppstår fel.

```Python
with open("textfil.txt", "w") as file:
    file.write("En textfil som skapades med hjälp av Python.")
```

En annan användbar funktion är "sys" biblioteket, som ger oss möjlighet att skriva till en fil utan att behöva öppna eller stänga den. Detta är speciellt användbart när vi arbetar med stora mängder data.

```Python
import sys

sys.stdout = open("mangd.txt", "w")
print("Denna text kommer att skrivas till filen istället för att visas i terminalen.")
```

## Se även

Här är några användbara resurser för att lära dig mer om att skriva textfiler i Python:

- [Python dokumentation om filhantering](https://docs.python.org/sv/3/tutorial/inputoutput.html#reading-and-writing-files)
- [En detaljerad guide om att arbeta med textfiler i Python](https://realpython.com/read-write-files-python/)
- [Användbara exempel på hur man kan automatisera textbaserade uppgifter med Python](https://towardsdatascience.com/making-python-programs-stick-with-file-input-and-output-90a54a8b909a)