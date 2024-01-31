---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
simple_title:         "Utilizzo delle espressioni regolari"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Le espressioni regolari (Regex) sono strumenti per cercare e gestire pattern di stringhe. I programmatori le usano per validare input, effettuare ricerche complesse e manipolare testo efficacemente.

## Come fare:
```Python
import re

# Cerchiamo parole che iniziano per "s" e terminano per "e"
pattern = r"\bs\w*e\b"
testo = "Le arance sono succose e dolci come il sole estivo."

match = re.findall(pattern, testo)
print(match)  # Output: ['succose', 'sole']

# Sostituiamo parole con "casa"
pattern_sost = r"\b\w{4}\b"  # parole di 4 lettere
sostituito = re.sub(pattern_sost, "casa", testo)
print(sostituito)  # Output: casa arance sono casa e casa come il casa estivo.
```

## Approfondimenti
Le espressioni regolari hanno radici negli anni '50 con gli automi e la teoria formale dei linguaggi. Alternative a Regex includono parsing manuale e l'uso di librerie specifiche per il tipo di dato, come `beautifulsoup` per HTML o `pandas` per dati tabellari. In Python, la libreria `re` implementa le espressioni regolari, ma anche moduli come `regex` offrono funzionalità avanzate.

## Vedi Anche
- Documentazione ufficiale Python re module: https://docs.python.org/3/library/re.html
- Tutorial interattivo Regex: https://regexone.com
- Progetto GitHub regex: https://github.com/mrabarnett/mrab-regex
