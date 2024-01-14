---
title:    "Python: Skriva till standardfel"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av felsökning och felhantering i Python-programmering. Genom att skriva ut felmeddelanden och andra viktiga information till standard error istället för standard output, kan du enkelt skilja mellan vad som är normalt programutdata och eventuella felmeddelanden som du behöver åtgärda.

## Hur man gör det

För att skriva till standard error i Python använder du funktionen "sys.stderr.write()". Detta tillåter dig att skriva ut en textsträng till standard error. Nedan visar ett exempel på hur du skulle använda denna funktion:

```python
import sys

sys.stderr.write("Detta är ett felmeddelande som skrivs till standard error")
```

När du kör detta program kommer du se att textsträngen "Detta är ett felmeddelande som skrivs till standard error" skrivs ut till terminalen, men inte som en del av den vanliga programutdatan. Istället, om du skulle använda "sys.stdout.write()" skulle texten skrivas ut som en del av programutdatan.

## Djupdykning

I Python finns det två viktiga standarda värden: "sys.stdin" och "sys.stdout". Dessa representerar standard input och standard output för ditt program. Dessutom finns det också "sys.stderr", som är standard error. Detta är vanligtvis var dina felmeddelanden och annan viktig information skrivs ut.

Det är också värt att notera att standard error används för mer än bara felmeddelanden. Det är också där man skriver ut annan information som inte är en del av det normala programflödet, såsom statusuppdateringar eller instruktioner för användaren.

## Se även

* [Dokumentation för sys.stderr i Python](https://docs.python.org/3/library/sys.html#sys.stderr)
* [Felsökning i Python using stderr](https://realpython.com/python-logging/) (på engelska)
* [Skriva till standard error i terminalen](https://www.geeksforgeeks.org/redirecting-python-output-to-file-in-the-terminal/#error) (på engelska)