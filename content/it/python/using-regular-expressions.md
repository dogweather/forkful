---
title:    "Python: Utilizzare le espressioni regolari"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché
Le espressioni regolari sono uno strumento essenziale per i programmatori Python. Grazie ad esse, è possibile cercare e manipolare dati all'interno di stringhe in modo efficiente e preciso.

## Come utilizzarle
Per utilizzare espressioni regolari in Python, è necessario importare il modulo ```re```. Vediamo alcuni esempi pratici per comprendere meglio il loro funzionamento.

```Python
import re

# Trovare una corrispondenza in una stringa
stringa = "Benvenuti nel mondo della programmazione"
match = re.search("programmazione", stringa)
print(match)
```

L'output di questo codice sarà ```<_sre.SRE_Match object; span=(31, 43), match='programmazione'>```, indicando che è stata trovata una corrispondenza tra la stringa e l'espressione regolare specificata.

```Python
import re

# Sostituire una parte di una stringa
stringa = "Ciao, come stai?"
nuova_stringa = re.sub("stai", "va?", stringa)
print(nuova_stringa)
```

In questo esempio, l'output sarà ```Ciao, come va?```, in quanto viene sostituita la parte "stai" con "va?" nella stringa originale.

## Approfondimento
Le espressioni regolari offrono una vasta gamma di simboli e operatori per costruire pattern ancora più precisi. Alcuni di questi includono:
- Il punto ```.```, che rappresenta qualsiasi carattere tranne il punto e a capo
- I caratteri speciali come ```^```, ```$```, ```*```, ```+```, che consentono di effettuare ricerche più specifiche, ad esempio cercando delle parole che iniziano o finiscono con un determinato carattere
- Le parentesi ```()``` e gli operatori logici come ```|```, che consentono di creare gruppi di caratteri e definire alternative nei pattern

È importante notare che le espressioni regolari sono sensibili alle maiuscole e minuscole, a meno che non si utilizzi il flag ```re.IGNORECASE``` durante la ricerca.

## Vedi anche
- [Python Regular Expression Tutorial](https://www.python-course.eu/python3_re.php)
- [Regular Expression Operations in Python](https://www.tutorialspoint.com/python3/python_reg_expressions.htm)
- [Python re Module - Official Documentation](https://docs.python.org/3/library/re.html)