---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "Come fare: Faccio abbastanza spesso questo da averlo rifattorizzato\
  \ in questa semplice funzione `delete()`. \xC8 anche una buona dimostrazione dei\u2026"
lastmod: '2024-04-05T21:53:43.770160-06:00'
model: gpt-4-0125-preview
summary: Faccio abbastanza spesso questo da averlo rifattorizzato in questa semplice
  funzione `delete()`.
title: Eliminazione dei caratteri corrispondenti a un pattern
weight: 5
---

## Come fare:
```Python
import re

# Stringa di esempio
testo = "Ciao, Mondo! 1234"

# Rimuovi tutti i numeri
senza_numeri = re.sub(r'\d', '', testo)
print(senza_numeri)  # Output: "Ciao, Mondo! "

# Rimuovi la punteggiatura
senza_punteggiatura = re.sub(r'[^\w\s]', '', testo)
print(senza_punteggiatura)  # Output: "Ciao Mondo 1234"

# Rimuovi le vocali
senza_vocali = re.sub(r'[aeiouAEIOU]', '', testo)
print(senza_vocali)  # Output: "C, Mnd! 1234"
```

### La mia funzione personalizzata

Faccio abbastanza spesso questo da averlo rifattorizzato in questa semplice funzione `delete()`. È anche una buona dimostrazione dei [doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(stringa: str, regex: str) -> str:
    """
    >>> delete("Ciao, mondo!", "l")
    'Ciao, mondo!'

    >>> delete("Ciao, mondo!", "[a-z]")
    'C, !'
    """
    return re.sub(regex, "", stringa)
```



## Approfondimento
La pratica di eliminare caratteri che corrispondono a un pattern in un testo ha radici profonde nell'informatica, risalendo agli strumenti Unix come `sed` e `grep`. In Python, il modulo `re` fornisce questa capacità, sfruttando le espressioni regolari - uno strumento potente e versatile per l'elaborazione del testo.

Alternative al modulo `re` includono:
- Metodi delle stringhe come `replace()` per casi semplici.
- Librerie di terze parti come `regex` per pattern più complessi e migliore supporto Unicode.

Sotto il cofano, quando si utilizza `re.sub()`, l'interprete Python compila il pattern in una serie di bytecode, processati da una macchina a stati che esegue il matching di pattern direttamente sul testo in input. Questa operazione può essere dispendiosa in termini di risorse per stringhe grandi o pattern complessi, quindi le considerazioni sulla performance sono cruciali per l'elaborazione di grandi dati.

## Vedi Anche
- [Documentazione del modulo `re` di Python](https://docs.python.org/3/library/re.html): Documenti ufficiali per le espressioni regolari in Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Una guida completa alle espressioni regolari.
- [Tutorial di Real Python su regex](https://realpython.com/regex-python/): Applicazioni pratiche delle espressioni regolari in Python.
