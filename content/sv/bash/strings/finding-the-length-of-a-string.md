---
date: 2024-01-20 17:46:49.204189-07:00
description: "Hur g\xF6r man: Du kan anv\xE4nda `${#str\xE4ng}` f\xF6r att f\xE5 en\
  \ str\xE4ngs l\xE4ngd."
lastmod: '2024-03-13T22:44:38.071694-06:00'
model: gpt-4-1106-preview
summary: "Du kan anv\xE4nda `${#str\xE4ng}` f\xF6r att f\xE5 en str\xE4ngs l\xE4ngd."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

## Hur gör man:
Du kan använda `${#sträng}` för att få en strängs längd.

```Bash
sträng="Hej, Sverige!"
längd=${#sträng}
echo "Strängens längd är: $längd"
```

Sample output:

```
Strängens längd är: 13
```

## Fördjupning:
Historiskt sett har längden på en sträng varit viktig i många programmeringsområden, från att skapa textbaserade användargränssnitt till att analysera datamängder. I Bash, `${#sträng}` är ett enkelt och direkt sätt att få strängens längd, men det finns också kommandon som `expr` och `awk` för att uppnå samma sak med mer komplexitet. Implementationen av `${#sträng}` är direkt kopplad till Bash's hantering av strängvariabler och är optimerad för prestanda och enkelhet.

## Se även:
- [Bash String Manipulation Guide](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)
- [Bash scripting cheatsheet](https://devhints.io/bash)
