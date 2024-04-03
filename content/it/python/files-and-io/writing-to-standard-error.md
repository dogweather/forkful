---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:11.475246-07:00
description: 'Come fare: #.'
lastmod: '2024-03-13T22:44:43.017482-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Scrivere sull'errore standard
weight: 25
---

## Come fare:


### Utilizzando `sys.stderr`
Il modulo integrato `sys` di Python consente di scrivere esplicitamente su `stderr`. Questo approccio è diretto per messaggi di errore semplici o diagnostici.

```python
import sys

sys.stderr.write('Errore: Qualcosa è andato storto.\n')
```
Esempio di output (su stderr):
```
Errore: Qualcosa è andato storto.
```

### Utilizzando la funzione `print`
La funzione `print` di Python può reindirizzare il suo output su `stderr` specificando il parametro `file`. Questo metodo è utile per sfruttare la facilità d'uso di `print` mentre si gestiscono messaggi di errore.
```python
from sys import stderr

print('Errore: Fallimento nel modulo.', file=stderr)
```
Esempio di output (su stderr):
```
Errore: Fallimento nel modulo.
```

### Utilizzando il modulo `logging`
Per una soluzione più completa, il modulo `logging` di Python può indirizzare i messaggi su `stderr` e molto altro, come scrivere su un file o personalizzare il formato dei messaggi. Questo metodo è il migliore per applicazioni che richiedono vari livelli di registrazione, formattazione dei messaggi o destinazioni.
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Errore: Connessione al database fallita.')
```
Esempio di output (su stderr):
```
ERROR:__main__:Errore: Connessione al database fallita.
```

### Librerie di terze parti: `loguru`
`loguru` è una popolare libreria di terze parti che semplifica il logging nelle applicazioni Python. Dirotta automaticamente gli errori su `stderr`, tra le altre caratteristiche.

Per utilizzare `loguru`, installalo prima via pip:
```shell
pip install loguru
```

Poi, incorporalo nel tuo script Python come segue:
```python
from loguru import logger

logger.error('Errore: Impossibile aprire il file.')
```
Esempio di output (su stderr):
```
2023-04-05 12:00:00.000 | ERRORE    | __main__:<module>:6 - Errore: Impossibile aprire il file.
```
