---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog existerar i Python innebär att verifiera om en viss mapp finns på en viss plats i filsystemet. Programmerare gör detta för att undvika filfel och säkerställa korrekt datahantering. 

## Hur gör man:
För att kontrollera om en katalog existerar i Python använder vi `os`-modulen och dess `path.exists`-metod. Här är en exempelkod:

```Python
import os

def kolla_katalog(path):
    if os.path.exists(path):
        print("Katalogen existerar.")
    else:
        print("Katalogen existerar inte.")

kolla_katalog("/din/väg/här")
```
Om katalogen finns kommer programmet skriva ut "Katalogen existerar" i konsolen, annars "Katalogen existerar inte."

## Djupdykning
Historiskt sett var man tidigare tvungen att försöka öppna katalogen och hantera eventuella fel för att kontrollera om en katalog existerade. Det var bökigt. Metoden `os.path.exists` introducerades som en förbättring och är nu standardmetoden.

Som alternativ finns också `os.path.isdir()`-funktionen, men den returnerar `False` både när katalogen inte existerar och när sökvägen är till en fil, inte en katalog.

En viktig detalj att känna till är att `os.path.exists` faktiskt kollar både om katalogen och filer existerar. Om du vill vara säker på att det är en katalog, använd den mer specifika `os.path.isdir()`.

## Se också
1. Python `os`-modulens officiella dokumentation: https://docs.python.org/3/library/os.html
2. Skillnaden mellan `os.path.exists` och `os.path.isdir`: https://stackoverflow.com/questions/82831/how-do-i-check-whether-a-file-exists-without-exceptions