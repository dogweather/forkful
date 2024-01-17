---
title:                "Utilizzando le espressioni regolari"
html_title:           "Python: Utilizzando le espressioni regolari"
simple_title:         "Utilizzando le espressioni regolari"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Le espressioni regolari (o regex) sono una potente funzionalità di Python che consente ai programmatori di cercare e manipolare stringhe di testo in modo efficiente. L'utilizzo di espressioni regolari può sembrare intimidatorio per chiunque sia nuovo alla programmazione, ma una volta imparato, diventeranno uno strumento indispensabile per gestire grandi quantità di dati in modo rapido ed efficace.

## Come fare:
Per utilizzare le espressioni regolari in Python, dobbiamo importare il modulo `re`. Dopodiché possiamo utilizzare la funzione `search()` per cercare un modello all'interno di una stringa e la funzione `findall()` per trovare tutte le occorrenze di quel modello. Di seguito è riportato un esempio di come trovare tutte le vocali in una stringa utilizzando le espressioni regolari in Python:

```Python
import re

stringa = "Ciao mondo"
vocali = re.findall("[aeiou]", stringa)
print(vocali)

# Output:
# ['i', 'a', 'o', 'o']
```

## Approfondimento:
Le espressioni regolari sono state introdotte nel linguaggio di programmazione Perl negli anni '80 e sono diventate popolari dagli anni '90, grazie alla loro potenza e flessibilità. Oggi sono comuni in vari linguaggi di programmazione, incluso Python.

In alternativa alle espressioni regolari, è possibile utilizzare la funzione `split()` di Python per suddividere una stringa in base a un determinato delimitatore, oppure il metodo `replace()` per sostituire una parte di una stringa con un'altra. Tuttavia, le espressioni regolari offrono una maggiore flessibilità e precisione nei confronti dei pattern cercati.

Per implementare le espressioni regolari in Python, il modulo `re` utilizza la libreria C di espressioni regolari, che consente di utilizzare un linguaggio specifico per descrivere i modelli da cercare. Ciò significa che i programmatori possono utilizzare le stesse espressioni regolari in diversi linguaggi di programmazione, rendendole uno strumento versatile e potente per gestire i dati.

## Vedi anche:
- [Documentazione delle espressioni regolari di Python](https://docs.python.org/3/library/re.html)
- [Un tutorial interattivo sulle espressioni regolari in Python](https://regexone.com/references/python) 
- [Un confronto tra espressioni regolari e altri metodi di manipolazione delle stringhe in Python](https://realpython.com/python-re-gex/)