---
title:                "Generering av slumpmässiga nummer"
html_title:           "Python: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att generera slumpmässiga nummer är en viktig del av programmering, och det handlar helt enkelt om att skapa nummer som inte följer ett förutbestämt mönster eller regel. Programrerare använder slumpmässiga nummer för att skapa variation och realistiska simuleringar, samt för att öka säkerheten i kryptering och spelapplikationer.

## Hur gör man:
Här är ett enkelt exempel på hur man kan använda sig av den inbyggda funktionen "random" i Python för att generera ett slumpmässigt heltal mellan 1 och 10:

```Python
import random
num = random.randint(1, 10)
print(num)
```

Den här kodraden kommer att producera ett slumpmässigt heltal varje gång den körs, vilket kan vara användbart för att skapa variation i spel eller simuleringar.

## Djupdykning:
Slumpmässiga nummer har använts inom matematiken under lång tid, men det var först på 1900-talet som man började utveckla datorprogram för att skapa dem. En alternativ metod för att generera slumpmässiga nummer är att använda sig av "pseudoslump", vilket skapar mönsterade nummer genom en matematisk beräkning. Dock är dessa nummer inte helt slumpmässiga utan följer en viss ordning.

I Python använder man sig av en algoritm kallad "Mersenne Twister" för att skapa slumpmässiga nummer. Denna algoritm är väldigt effektiv och genererar högkvalitativa slumpmässiga nummer med få upprepningar.

## Se också:
- [Dokumentation för "random" i Python](https://docs.python.org/3/library/random.html)
- [Artikel om pseudoslump i Wall Street Journal (engelska)](https://www.wsj.com/articles/the-pseudo-random-method-in-computer-science-analogous-to-modernism-1423813258)