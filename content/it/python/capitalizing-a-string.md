---
title:                "Maiuscolare una stringa."
html_title:           "Python: Maiuscolare una stringa."
simple_title:         "Maiuscolare una stringa."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
Capitalize è una funzione utile in Python per rendere una stringa maiuscola, utile per formattare testi o per garantire una corretta sintassi in alcune situazioni.

## Come fare
Utilizzare la funzione `capitalize()` è molto semplice, basta chiamarla su una stringa e il primo carattere verrà reso maiuscolo. 

```Python
testo = "questo è un esempio"
nuovo_testo = testo.capitalize()
print(nuovo_testo)
```
Output: "Questo è un esempio"

Si può anche specificare un indice di inizio e fine per capitilare solo una parte della stringa.

```Python
testo = "questo è un esempio"
nuovo_testo = testo.capitalize(8, 12)
print(nuovo_testo)
```
Output: "questo È un Esempio"

## Approfondimenti
La funzione `capitalize()` può essere utile anche per convertire le iniziali delle parole di una stringa in maiuscolo. Tuttavia, bisogna prestare attenzione perché non funziona correttamente con parole già in maiuscolo.

Un altro modo per capitalizzare una stringa è utilizzare il metodo `title()`, che trasforma ogni parola iniziale in maiuscolo.

```Python
testo = "questo è un esempio"
nuovo_testo = testo.title()
print(nuovo_testo)
```
Output: "Questo È Un Esempio"

Ad esempio, se si vuole capitalizzare solo la prima lettera di ogni parola, si può utilizzare il modulo `string.capwords()`.

```Python
import string

testo = "questa è una frase di prova"
nuovo_testo = string.capwords(testo)
print(nuovo_testo)
```
Output: "Questa È Una Frase Di Prova"

## Vedi anche
- Documentazione ufficiale di Python sulla funzione `capitalize()`: https://docs.python.org/3/library/stdtypes.html#str.capitalize
- Tutorial su come manipolare le stringhe in Python: https://realpython.com/python-strings/
- Approfondimenti sulle diverse funzioni di formattazione delle stringhe in Python: https://www.askpython.com/python/examples/format-strings-in-python