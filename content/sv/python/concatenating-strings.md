---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/concatenating-strings.md"
---

{{< edit_this_page >}}

---

## Vad & Varför?

Sammanfogning av strängar innebär att man kopplar ihop två eller fler strängar till en enda. Programmerare gör det för att skapa dynamiska meddelanden, bygga komplexa strängar baserat på användarinput, och mycket mer.

## Hur man gör:

Vi kan sammanfoga strängar på flera sätt i Python. Nedan visas några exempel:

```python
# Metod 1: Plusoperator
str1 = 'Hej'
str2 = 'Värld'
str3 = str1 + ' ' + str2
print(str3)  # Output: "Hej Värld"

# Metod 2: F-string
name = 'Anna'
msg = f'Hej, {name}!'
print(msg)  # Output: "Hej, Anna!"

# Metod 3: str.join() funktion
words = ['python', 'är', 'kul']
sentence = ' '.join(words)
print(sentence)  # Output: "python är kul"
```

## Djup Dykning:

Historiskt sett har strängsammanfogning i Python genomgått flera förändringar. I Python 2 använde vi %-formattering. Från och med Python 3.6 introducerades f-strängar som ett mer lättläst och flexibelt alternativ.

```python
# Python 2: Procentformattering
name = 'Anna'
msg = 'Hej, %s!' % name
print(msg)  # Output: "Hej, Anna!"
```

Som alternativ kan du använda `format()` metod. Dock, f-strängar är betydligt lättare att läsa och skriva.

```python
# Alternativ: format() metod
name = 'Anna'
msg = 'Hej, {}!'.format(name)
print(msg)  # Output: "Hej, Anna!"
```

Vad gäller implementeringsdetaljer är f-strängar och `str.join()` vanligtvis snabbare än strängsammanfogning med "+"-operatorn, särskilt för stora strängar.

## Se Även:

De officiella Python-dokumenten ger mer information om [strängformattering](https://docs.python.org/3/reference/lexical_analysis.html#f-strings) och [strängmetoder](https://docs.python.org/3/library/stdtypes.html#string-methods).

För att förbättra dina Python-färdigheter, kolla in dessa handledningar på [Real Python](https://realpython.com/python-f-strings/) och [Python For Beginners](https://www.pythonforbeginners.com/concatenation/string-concatenation-and-formatting-in-python).