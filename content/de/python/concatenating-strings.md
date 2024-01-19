---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/concatenating-strings.md"
---

{{< edit_this_page >}}

# Python String-Konkatenation: Wie, Was und Warum?

## Was & Warum?
String-Konkatenation ist der Prozess, bei dem zwei oder mehr Strings zu einem einzigen String zusammengefügt werden. Programmierer nutzen dies, um verschiedene Informationen in einer lesbaren und verständlichen Ausgabe zu kombinieren.

## Wie macht man das:
```Python
# Beispiel 1: Mit dem Plus Operator
string1 = 'Hallo'
string2 = 'Welt'
concatenated = string1 + ', ' + string2
print(concatenated)  # Ausgabe: Hallo, Welt

# Beispiel 2: Mit der join()-Funktion
satzteile = ['Ich', 'liebe', 'Python']
concatenated = ' '.join(satzteile)
print(concatenated)  # Ausgabe: Ich liebe Python
```

## Vertiefen
Die Methode der String-Konkatenation ist ein fundamentales Merkmal der meisten modernen Programmiersprachen und hat eine lange Historie, die bis zu den Anfängen des Programmierens zurückreicht.

Alternativen zur String-Konkatenation in Python sind die string interpolation oder f-Strings, wie im folgenden Beispiel demonstriert:

```Python
name = 'Anna'
alter = 25
melding = f'Mein Name ist {name} und ich bin {alter} Jahre alt.'
print(melding)  # Ausgabe: Mein Name ist Anna und ich bin 25 Jahre alt.
```
Obwohl f-Strings in vielen Fällen weniger einschüchternd und bequemer sein können, sollten sie nicht als Ersatz für traditionelle Methoden der String-Konkatenation betrachtet werden. Beide haben ihre Rolle in Python und bieten flexible Möglichkeiten, Strings für verschiedene Programmierbedürfnisse zu manipulieren.

## Siehe Auch
Für weiterführende Informationen zur String-Konkatenation, consider these Quellen:

[Python String Concatenation Tutorial](https://www.datacamp.com/community/tutorials/python-string-concatenation)

[Python String join() Method](https://www.w3schools.com/python/ref_string_join.asp)

[Python f-Strings Guide](https://realpython.com/python-f-strings/)