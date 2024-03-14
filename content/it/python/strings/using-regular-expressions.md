---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:51.552559-07:00
description: "Le espressioni regolari (regex) sono modelli utilizzati per individuare\
  \ combinazioni di caratteri nelle stringhe. I programmatori le utilizzano per\u2026"
lastmod: '2024-03-13T22:44:42.991373-06:00'
model: gpt-4-0125-preview
summary: "Le espressioni regolari (regex) sono modelli utilizzati per individuare\
  \ combinazioni di caratteri nelle stringhe. I programmatori le utilizzano per\u2026"
title: Utilizzo delle espressioni regolari
---

{{< edit_this_page >}}

## Cos'è e perché?
Le espressioni regolari (regex) sono modelli utilizzati per individuare combinazioni di caratteri nelle stringhe. I programmatori le utilizzano per cercare, modificare o manipolare testo basandosi su schemi definiti, rendendole indispensabili per compiti come la validazione dei dati, l'analisi sintattica o la trasformazione.

## Come fare:
Utilizzare le regex in Python comporta l'uso del modulo `re`, che fornisce un insieme di funzioni per elaborare testo utilizzando espressioni regolari.

### Corrispondenza di modelli di base
Per cercare un modello in una stringa, usare `re.search()`. Restituisce un oggetto corrispondenza quando il modello viene trovato, altrimenti `None`.
```python
import re

text = "Learn Python programming"
match = re.search("Python", text)
if match:
    print("Modello trovato!")
else:
    print("Modello non trovato.")
```
Output:
```
Modello trovato!
```

### Compilazione di espressioni regolari
Per l'uso ripetuto dello stesso modello, compilarlo prima con `re.compile()` per una migliore prestazione.
```python
pattern = re.compile("Python")
match = pattern.search("Learn Python programming")
if match:
    print("Modello compilato trovato!")
```
Output:
```
Modello compilato trovato!
```

### Suddivisione delle stringhe
Per suddividere una stringa in ogni corrispondenza di un modello regex, usare `re.split()`.
```python
result = re.split("\s", "Python is fun")
print(result)
```
Output:
```
['Python', 'is', 'fun']
```

### Trovare tutte le corrispondenze
Per trovare tutte le occorrenze non sovrapposte di un modello, usare `re.findall()`.
```python
matches = re.findall("n", "Python programming")
print(matches)
```
Output:
```
['n', 'n']
```

### Sostituire testo
Usare `re.sub()` per sostituire le occorrenze di un modello con una nuova stringa.
```python
replaced_text = re.sub("fun", "awesome", "Python is fun")
print(replaced_text)
```
Output:
```
Python is awesome
```

### Librerie di terze parti
Sebbene il modulo `re` integrato in Python sia potente, le librerie di terze parti come `regex` offrono più funzionalità e una prestazione migliorata. Per usare `regex`, installarlo tramite pip (`pip install regex`) e importarlo nel codice.

```python
import regex

text = "Learning Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"Versione trovata: {match.group(1)}")
```
Output:
```
Versione trovata: 3.8
```
