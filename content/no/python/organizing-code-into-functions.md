---
title:                "Organisering av kode i funksjoner"
aliases:
- no/python/organizing-code-into-functions.md
date:                  2024-01-26T01:11:59.926827-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisering av kode i funksjoner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å organisere kode i funksjoner handler om å dele opp koden din i gjenbrukbare biter med spesifikke formål. Vi gjør det for å gjøre koden renere, enklere å lese, feilsøke, og oppdatere.

## Hvordan:
La oss si at du skriver et skript for å beregne kvadratet og kuben av et tall. Uten funksjoner blir det et rot av gjentakelse:

```Python
nummer = 4
kvadrat = nummer * nummer
kube = nummer * nummer * nummer
print(f"Kvadrat: {kvadrat}, kube: {kube}")

nummer = 5
kvadrat = nummer * nummer
kube = nummer * nummer * nummer
print(f"Kvadrat: {kvadrat}, kube: {kube}")
```
Resultat:
```
Kvadrat: 16, kube: 64
Kvadrat: 25, kube: 125
```

Med funksjoner blir det ryddigere:

```Python
def kvadrat(n):
    return n * n

def kube(n):
    return n ** 3

nummer = 4
print(f"Kvadrat: {kvadrat(nummer)}, kube: {kube(nummer)}")

nummer = 5
print(f"Kvadrat: {kvadrat(nummer)}, kube: {kube(nummer)}")
```
Resultat:
```
Kvadrat: 16, kube: 64
Kvadrat: 25, kube: 125
```

## Dypdykk
Tilbake i tiden når programmer var enkle, kunne man klare seg med bare å skrive en liste med instruksjoner. Men ettersom programvaren ble mer kompleks, innså utviklerne at de skrev samme kode om og om igjen. Hallo, funksjoner—gjenbrukbare blokker med kode som utfører en enkelt handling.

Alternativer til funksjoner inkluderer klasser (som grupperer funksjoner med dataene de opererer på) og innebygd kode (intelligens akkurat der du trenger den, men risikabelt for komplekse oppgaver). Når det gjelder implementering, er trikset ikke bare å skape funksjoner, men å gjøre dem godt på en ting—tenk prinsippet om enkeltansvar. Funksjoner bør også ideelt sett være tilstandsløse, noe som betyr ingen overraskelser med data som kommer inn eller går ut.

## Se også
- De offisielle Python-tutorialene om funksjoner: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 'Clean Code' av Robert C. Martin, for prinsipper om hvordan man skriver rene funksjoner.
- 'Refactoring: Improving the Design of Existing Code' av Martin Fowler, som inkluderer eksempler på organisering av kode.
