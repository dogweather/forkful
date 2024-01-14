---
title:                "Python: Ekstrahering av substringer"
simple_title:         "Ekstrahering av substringer"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang ønsket å hente ut en del av en tekststreng? Å kunne ekstrahere substrings, eller deler av en tekst, er en nyttig funksjon i Python som kan hjelpe deg med å manipulere data på en mer effektiv måte. Uansett om du skal jobbe med tekstbehandling, dataanalyse eller automatisering, kan kunnskap om å ekstrahere substrings være svært nyttig.

## Hvordan

Det første du må gjøre er å importere `re` biblioteket, som står for "regular expressions" eller regulære uttrykk på norsk. Dette biblioteket er viktig for å kunne finne og håndtere tekststrenger på en mer avansert måte. Deretter kan du bruke funksjonen `re.search()` for å finne en spesifikk substring i en tekststreng.

```Python
import re

tekst = "Jeg elsker å programmere i Python!"

substring = re.search("programmere", tekst) 

print(substring)
```

Output:

```
<re.Match object; span=(10, 20), match='programmere'>
```

Her ser vi at funksjonen `re.search()` har funnet det vi lette etter, nemlig ordet "programmere" i tekststrengen. For å hente ut selve substringen, kan vi bruke metoden `group()` på objektet som blir returnert:

```Python
print(substring.group())
```

Output:

```
programmere
```

Du kan også bruke regulære uttrykk for å finne mer komplekse substrings. For eksempel, hvis vi ønsker å finne alle ordene som starter med bokstaven "p" i teksten vår, kan vi bruke uttrykket `p\w+`:

```Python
import re

tekst = "Python er et fantastisk programmeringsspråk!"

substring = re.search("p\w+", tekst) 

print(substring.group())
```

Output:

```
Python
```

## Dypdykk

Det finnes mange ulike metoder og uttrykk du kan bruke for å ekstrahere substrings i Python. Du kan for eksempel bruke metoden `findall()` for å finne alle forekomster av en substring, eller `split()` for å dele opp en tekststreng i ulike deler.

Videre kan du også bruke symbolske uttrykk for å finne spesifikke mønstre i teksten, som for eksempel å finne alle ord som slutter på en bestemt bokstav. Det finnes også mange nyttige biblioteker og pakker som kan hjelpe deg med å håndtere tekst på en mer avansert måte, som for eksempel `nltk` (Natural Language Toolkit) eller `gensim`.

## Se også

- [Introduction to Regular Expressions in Python](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)
- [Python Regular Expressions: from Beginner to Advanced](https://medium.com/factory-mind/python-regular-expression-tutorial-from-beginner-to-advanced-417dd68658f5)
- [Mastering Python Regular Expressions](https://masteringpythonregularexpressions.gruppioni.eu/)
- [How to Extract Data From Websites in Python Using RegEx](https://www.digitalocean.com/community/tutorials/how-to-extract-data-from-websites-in-python-using-regex)