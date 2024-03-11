---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:05.346157-07:00
description: "Regulj\xE4ra uttryck (regex) \xE4r m\xF6nster som anv\xE4nds f\xF6r\
  \ att matcha teckenkombinationer i str\xE4ngar. Programmerare anv\xE4nder dem f\xF6\
  r att s\xF6ka, redigera eller\u2026"
lastmod: '2024-03-11T00:14:10.783741-06:00'
model: gpt-4-0125-preview
summary: "Regulj\xE4ra uttryck (regex) \xE4r m\xF6nster som anv\xE4nds f\xF6r att\
  \ matcha teckenkombinationer i str\xE4ngar. Programmerare anv\xE4nder dem f\xF6\
  r att s\xF6ka, redigera eller\u2026"
title: "Att anv\xE4nda regulj\xE4ra uttryck"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck (regex) är mönster som används för att matcha teckenkombinationer i strängar. Programmerare använder dem för att söka, redigera eller manipulera text baserat på definierade mönster, vilket gör dem oumbärliga för uppgifter som datavalidering, tolkning eller transformation.

## Hur:
Att använda regex i Python innebär att använda `re`-modulen, som tillhandahåller en uppsättning funktioner för att bearbeta text med hjälp av reguljära uttryck.

### Grundläggande mönstersökning
För att söka efter ett mönster i en sträng, använd `re.search()`. Den returnerar ett matchningsobjekt när mönstret hittas, annars `None`.
```python
import re

text = "Lär dig programmera Python"
match = re.search("Python", text)
if match:
    print("Mönster hittat!")
else:
    print("Mönster hittades inte.")
```
Utskrift:
```
Mönster hittat!
```

### Kompilera reguljära uttryck
För upprepad användning av samma mönster, kompilera det först med `re.compile()` för bättre prestanda.
```python
pattern = re.compile("Python")
match = pattern.search("Lär dig programmera Python")
if match:
    print("Kompilerat mönster hittat!")
```
Utskrift:
```
Kompilerat mönster hittat!
```

### Dela upp strängar
För att dela upp en sträng vid varje matchning av ett regex-mönster, använd `re.split()`.
```python
result = re.split("\s", "Python är kul")
print(result)
```
Utskrift:
```
['Python', 'är', 'kul']
```

### Hitta alla matchningar
För att hitta alla icke-överlappande förekomster av ett mönster, använd `re.findall()`.
```python
matches = re.findall("n", "Python programmering")
print(matches)
```
Utskrift:
```
['n', 'n']
```

### Ersätta text
Använd `re.sub()` för att ersätta förekomster av ett mönster med en ny sträng.
```python
replaced_text = re.sub("kul", "fantastiskt", "Python är kul")
print(replaced_text)
```
Utskrift:
```
Python är fantastiskt
```

### Tredjepartslbibliotek
Även om Pythons inbyggda `re`-modul är kraftfull, erbjuder tredjepartsbibliotek som `regex` fler funktioner och förbättrad prestanda. För att använda `regex`, installera det via pip (`pip install regex`) och importera det i din kod.

```python
import regex

text = "Lär dig Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"Hittad version: {match.group(1)}")
```
Utskrift:
```
Hittad version: 3.8
```
