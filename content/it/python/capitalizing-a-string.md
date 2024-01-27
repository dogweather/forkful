---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Capitalizzare una stringa in Python significa convertire la prima lettera di ogni parola in maiuscolo, lasciando inalterate le altre. I programmatori lo fanno per normalizzare i dati testuali (come i titoli) o per rispettare convenzioni stilistiche.

## How to: (Come fare:)
```python
# Metodo .title()
testo = "ciao mondo"
testo_capitalizzato = testo.title()
print(testo_capitalizzato)  # Ciao Mondo

# Metodo .capitalize()
testo_2 = "buon giorno"
testo_2_capitalizzato = testo_2.capitalize()
print(testo_2_capitalizzato)  # Buon giorno

# Metodo .upper() su prima lettera e concatenazione
testo_3 = "sera"
testo_3_capitalizzato = testo_3[0].upper() + testo_3[1:]
print(testo_3_capitalizzato)  # Sera
```

## Deep Dive (Approfondimento)
Capitalizzare una stringa non è nato con Python. Deriva dalla necessità di presentare il testo in maniera formale, come nei titoli o all'inizio delle frasi. Alternative come `lower()` e `upper()` trasformano tutte le lettere in minuscole o maiuscole. Le implementazioni differiscono: `title()` può essere ingannato da apostrofi (es. "l'italia" diventa "L'Italia" ma "l'Italia" diventa "L'italia"), mentre `capitalize()` rende maiuscola solo la prima lettera della stringa. Python 3 ha migliorato il supporto per Unicode, rendendo la capitalizzazione di stringhe in altre lingue più accurata.

## See Also (Vedi Anche)
- Documentazione ufficiale di string methods in Python: https://docs.python.org/3/library/stdtypes.html#string-methods
- Guida alla formattazione delle stringhe: https://realpython.com/python-string-formatting/
- Unicode e lavorare con testi in Python: https://docs.python.org/3/howto/unicode.html
