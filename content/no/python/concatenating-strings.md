---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammensetting av strenger, eller 'string concatenation' på engelsk, er en aktivitet der vi binder sammen to eller flere strenger (tekst) for å skape en større streng. Programmører gjør dette for å kombinere eller manipulere tekst på en dynamisk og nøyaktig måte.

## Hvordan:

Python tilbyr flere metoder for å samsette strenger. Her er tre vanlige måter:

```Python
# Metode 1: Pluss operatør
navn = "Ola"
hilsen = "Hei, " + navn + "!"
print(hilsen) # Output: Hei, Ola!

# Metode 2: Format funksjon
hilsen = "Hei, {}!".format(navn)
print(hilsen) # Output: Hei, Ola!

# Metode 3: f-strenger (fra Python 3.6 og fremover)
hilsen = f"Hei, {navn}!"
print(hilsen) # Output: Hei, Ola!
```

## Dypdykk

Sammensetting av strenger har vært en del av programmering siden begynnelsen av høy-nivå språk. I Python, så ble 'f-strenger' eller 'f-strings' introdusert i versjon 3.6 som en raskere og mer lesbar metode for å samsette strenger.

Alternativer for sammensetting av strenger inkluderer metoder som `join()`, `%.formatting`, `string.Template` osv. Valget mellom disse alternativene kommer ofte ned til individuelle preferanser, ytelsesbehov og versjon av Python du bruker. For eksempel, `f-strings` tilbyr bedre ytelse enn de fleste andre metoder, men er bare tilgjengelig i Python 3.6 og nyere versjoner.

I Python, når du samensetter strenger, skaper Python faktisk nye strenger. Dette er fordi strenger er 'immutable' i Python, noe som betyr at de ikke kan endres etter at de er laget. Dette er noe å ha i bakhodet hvis du arbeider med store mengder tekst, da det kan påvirke ytelsen til programmet ditt.

## Se Også:

1. Python offisiell dokumentasjon om strenger: [Python Docs](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
2. Effektiv string concatenation i Python: [Python Wiki](https://wiki.python.org/moin/PythonSpeed/PerformanceTips#String_Concatenation)
3. Python's f-Strings: An Improved String Formatting Syntax (Guide): [Real Python](https://realpython.com/python-f-strings/)