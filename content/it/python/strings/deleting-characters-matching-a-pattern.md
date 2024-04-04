---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: 'Come fare: .'
lastmod: '2024-04-04T01:27:40.087177-06:00'
model: gpt-4-0125-preview
summary: .
title: Eliminare i caratteri corrispondenti a un modello
weight: 5
---

## Come fare:
```Python
import re

# Stringa di esempio
testo = "Ciao, Mondo! 1234"

# Rimuovere tutti i numeri
senza_numeri = re.sub(r'\d', '', testo)
print(senza_numeri)  # Output: "Ciao, Mondo! "

# Rimuovere la punteggiatura
senza_punteggiatura = re.sub(r'[^\w\s]', '', testo)
print(senza_punteggiatura)  # Output: "Ciao Mondo 1234"

# Rimuovere le vocali
senza_vocali = re.sub(r'[aeiouAEIOU]', '', testo)
print(senza_vocali)  # Output: "C, Mnd! 1234"
```

### Una funzione personalizzata che ho scritto

Faccio questo abbastanza frequentemente da averlo rifattorizzato in questa funzione `delete()`. È anche una buona dimostrazione di [doctests](https://docs.python.org/3/library/doctest.html):

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
La pratica di eliminare caratteri corrispondenti a un modello nel testo ha radici profonde nell'informatica, risalendo a strumenti Unix precoci come `sed` e `grep`. In Python, il modulo `re` fornisce questa capacità, sfruttando le espressioni regolari: uno strumento potente e versatile per l'elaborazione del testo.

Alternative al modulo `re` includono:
- Metodi di stringa come `replace()` per casi semplici.
- Librerie di terze parti come `regex` per modelli più complessi e un migliore supporto Unicode.

Sotto il cofano, quando si usa `re.sub()`, l'interprete Python compila il modello in una serie di bytecodes, elaborati da una macchina a stati che esegue il pattern-matching direttamente sul testo di input. Questa operazione può essere intensiva sulle risorse per stringhe grandi o modelli complessi, quindi le considerazioni sulla performance sono cruciali per l'elaborazione di grandi dati.

## Vedi anche
- [Documentazione modulo `re` Python](https://docs.python.org/3/library/re.html): Documentazione ufficiale per le espressioni regolari in Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Una guida completa alle espressioni regolari.
- [Tutorial di Real Python su regex](https://realpython.com/regex-python/): Applicazioni del mondo reale delle espressioni regolari in Python.
