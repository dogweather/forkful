---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Python String Interpolering: Hvordan gjør vi det, og hvorfor?

## Hva & Hvorfor?

Interpolering av en streng i Python innebærer å sette verdier fra variabler rett inn i en streng. Programmerere gjør dette for å lage mer dynamiske strenger uten å trenge å konstant sette sammen strenger og variabler.

## Slik gjør du det:

Python tilbyr flere metoder for å interpolere strenger, og vi skal se på to av dem: f-strengformat og `.format()` metoden. Her er noen eksempler:

```Python
# f-strengformatering
navn = "Ola"
verdi = 10
print(f"Hei {navn}, du har {verdi} poeng.")

# .format() metoden
print("Hei {}, du har {} poeng.".format(navn, verdi))
```
Output:
```
Hei Ola, du har 10 poeng.
Hei Ola, du har 10 poeng.
```
Begge eksempler gir samme resultat, men f-strenger er mer lesbare og krever mindre kode.

## Dyp Dykk

Interpolering av strenger har vært en del av Python siden begynnelsen, men f-strenger ble introdusert mye senere, i Python 3.6, for å gjøre det enklere og raskere å formatere strenger.

I tillegg til f-strenger og `.format()`, er det også prosentformatmetoden `%`, men den ansees som utdatert, og det er anbefalt å bruke de to første metodene.

Mens f-strengformat krever evaluering av uttrykk inne i strengene ved kjøretid, bruker `.format()` metoden tupler og ordbok for å sette inn verdiene i strengen. Derfor, selv om f-strenger gir bedre lesbarhet, kan `.format()` metoden gi mer kontroll over hvordan strengen blir formatert.

## Se Også

For mer informasjon om strenginterpolering i Python, kan disse kildene være nyttige:

1. Python offisielle dokumentasjon for f-strenger: [Link][1]
2. Python offisielle dokumentasjon for `.format()` metoden: [Link][2]
3. En god tutorial på Real Python: [Link][3]

[1]: https://docs.python.org/3/reference/lexical_analysis.html#f-strings
[2]: https://docs.python.org/3/library/stdtypes.html#str.format
[3]: https://realpython.com/python-f-strings/