---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att radera tecken som matchar ett mönster innebär att från en sträng ta bort alla tecken som stämmer med ett visst kriterium (mönstret). Programmers gör detta för att rensa obetydliga eller oönskade tecken och för att forma data på ett lättläsligt och standardiserat sätt.

## Hur man gör:
Textbehandlingen i Python är relativt enkel. Nedan finns ett exempel på hur du tar bort alla icke-alfanumeriska tecken från en sträng med Python:

```Python
import re
s = "Hej där! Hur mår du __idag__?"
clean_s = re.sub(r'\W+', '', s)
print(clean_s)
```
När du kör det här får du 'HejdrHurmårduidag' som output.

## Fördjupning
Mönstermatchning har varit en grundläggande princip i databehandling ända sedan tidiga kommandotolkar som UNIX sh och senare bash. Att rensa upp en sträng genom att ta bort oönskade tecken var något som Python ansåg vara kritiskt och införde tidigt i sin utveckling.

Ett alternativ till metoden ovan är att använda list comprehension, vilket kan vara snabbare om mönstret är enkelt.

```Python
s = "Hej där! Hur mår du __idag__?"
clean_s = ''.join(e for e in s if e.isalnum())
print(clean_s)
```
Denna kod ger samma output som tidigare exempel 'HejdrHurmårduidag'.

Python använder "re" biblioteket för att matcha mönster, vilket implementeras med hjälp av regular expressions, en mycket kraftfull textmanipulationsteknik.

## Se också
Python dokumentationen innehåller massor av bra information:

[Reguljära uttryck i Python](https://docs.python.org/3/library/re.html)

[Så här tar du bort tecken från en sträng i Python](https://www.journaldev.com/23763/python-remove-character-from-string)

[List comprehension i Python](https://docs.python.org/3/tutorial/datastructures.html#list-comprehensions)