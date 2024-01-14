---
title:    "Python: Radera tecken som matchar ett mönster"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett visst mönster kan vara användbart i många situationer, särskilt inom datahantering och textbehandling. Det kan också vara användbart för att effektivisera kod och göra den mer lättläst.

## Hur man gör det

För att ta bort tecken som matchar ett visst mönster i Python används funktionen `re.sub()`. Detta gör det möjligt att ersätta delar av en sträng som matchar ett visst reguljärt uttryck med en tom sträng, vilket i princip tar bort dem från den ursprungliga strängen.

Ett enkelt exempel på detta kan vara följande:

```Python
import re

text = "Detta är en exempeltext med lite nummer 123 och specialtecken!@$"
ny_text = re.sub(r'[0-9!@$]', '', text)

print(ny_text)
```

Detta kodsnutt kommer att skriva ut en ny sträng där alla nummer, utropstecken, dollartecken och at-tecken har tagits bort. Output blir då: "Detta är en exempeltext med lite nummer och specialtecken".

## Fördjupning

För de som är intresserade av att lära sig mer om reguljära uttryck och hur de kan användas för att ta bort tecken som matchar ett visst mönster, rekommenderas det att gå in på Python-dokumentationen för `re`-modulen. Där finns innehållsrik information om hur man skapar och använder reguljära uttryck i Python.

En annan användbar resurs för att lära sig mer om reguljära uttryck är Regular-Expressions.info, som innehåller många exempel och övningsuppgifter för att stärka förståelsen.

## Se även

* [Python-dokumentationen för `re`-modulen](https://docs.python.org/3/library/re.html)
* [Regular-Expressions.info](https://www.regular-expressions.info/)