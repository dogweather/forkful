---
date: 2024-01-20 17:35:44.480822-07:00
description: "Come fare: Prima dell'avvento del \"+\" per concatenare le stringhe,\
  \ vari linguaggi di programmazione utilizzavano i propri metodi, spesso meno intuitivi.\u2026"
lastmod: '2024-04-05T21:53:43.777491-06:00'
model: gpt-4-1106-preview
summary: Prima dell'avvento del "+" per concatenare le stringhe, vari linguaggi di
  programmazione utilizzavano i propri metodi, spesso meno intuitivi.
title: Concatenazione di stringhe
weight: 3
---

## Come fare:
```Python
# Concatenazione con l'operatore +
saluto = "Ciao"
nome = "Marco"
messaggio = saluto + ", " + nome + "!"
print(messaggio)  # Output: Ciao, Marco!

# Concatenazione con la funzione join()
nomi = ["Anna", "Luigi", "Sofia"]
elenco_nomi = ", ".join(nomi)
print("Benvenuti " + elenco_nomi + "!")  # Output: Benvenuti Anna, Luigi, Sofia!

# Concatenazione con le f-strings (Python 3.6+)
età = 30
descrizione = f"{nome} ha {età} anni."
print(descrizione)  # Output: Marco ha 30 anni.
```

## Deep Dive
Prima dell'avvento del "+" per concatenare le stringhe, vari linguaggi di programmazione utilizzavano i propri metodi, spesso meno intuitivi. In Python, oltre all'uso dell'operatore `+`, abbiamo anche il metodo `.join()` che è particolarmente efficiente con liste di stringhe. Le f-strings (introdotti in Python 3.6) offrono un modo moderno e leggibile per includere variabili e espressioni dentro stringhe. È importante ricordare che ogni operazione di concatenazione crea una nuova stringa, dato che le stringhe in Python sono immutabili, quindi per operazioni ripetute su grandissime quantità di testo, `join()` o stringhe multilinea possono essere più performanti.

## See Also
- Documentazione ufficiale Python su stringhe: [python.org](https://docs.python.org/3/library/string.html)
- Python Software Foundation: [pep-498](https://www.python.org/dev/peps/pep-0498/) per una spiegazione approfondita delle f-strings
- Stack Overflow: discussioni e suggerimenti sulla concatenazione di stringhe in Python [stackoverflow.com](https://stackoverflow.com/questions/tagged/string-concatenation+python)
