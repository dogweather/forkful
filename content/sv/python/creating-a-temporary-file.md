---
title:    "Python: Skapa en tillfällig fil"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför 
Att skapa en temporär fil kan vara användbart när du behöver utföra en kortlivad uppgift eller när du inte vill skriva till en permanent fil. Temporära filer kan också användas för att hålla känslig information säker, då de automatiskt raderas när de inte längre behövs.

## Hur man gör det 
För att skapa en temporär fil i Python, används ```tempfile``` modulen. Först importerar man modulen: 
```Python
import tempfile
```
Sedan kan man använda ```tempfile.NamedTemporaryFile()``` för att skapa en temporär fil, till exempel: 
```Python
with tempfile.NamedTemporaryFile() as tmp_file: 
    print("Min temporary fil är: ", tmp_file.name)
```
Kör man koden ovan kommer man få följande output: 
```
Min temporary fil är: /tmp/tmptcaf444e
```
Som default skapas en temporär fil i "tmp" mappen på datorn, men man kan också specificera en annan mapp. 
```Python
with tempfile.NamedTemporaryFile(dir = "/hem/användare/temp") as tmp_file: 
    print("Min temporary fil är: ", tmp_file.name)
```
Om man kör koden ovan så kommer den temporära filen att skapas i "temp" mappen i användarens hemkatalog. 

## Djupdykning 
Vad händer egentligen när man skapar en temporär fil i Python? När man använder ```tempfile``` modulen så skapas en fil på disk och dess namn returneras. När filen inte längre behövs, så stängs den automatiskt och tas bort från systemet. Om man behöver behålla filen längre än dess livslängd, så finns det möjlighet att flytta filen till en permanent plats genom att använda funktionen ```tmp_file.flush()``` och sedan använda ```shutil``` för att kopiera filen till en annan mapp.

## Se även 
- [Tempfile modulen dokumentation på Pythons hemsida](https://docs.python.org/3/library/tempfile.html)
- [Datacamp tutorial om att skapa och hantera temporära filer i Python](https://www.datacamp.com/community/tutorials/working-temporary-files-python)