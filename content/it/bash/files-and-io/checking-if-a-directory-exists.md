---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:41.913627-07:00
description: "Come fare: Nel suo nucleo, Bash ti permette di controllare l'esistenza\
  \ di una directory utilizzando dichiarazioni condizionali e l'operatore `-d`. Di\u2026"
lastmod: '2024-03-13T22:44:43.613889-06:00'
model: gpt-4-0125-preview
summary: Nel suo nucleo, Bash ti permette di controllare l'esistenza di una directory
  utilizzando dichiarazioni condizionali e l'operatore `-d`.
title: Verifica se una directory esiste
weight: 20
---

## Come fare:
Nel suo nucleo, Bash ti permette di controllare l'esistenza di una directory utilizzando dichiarazioni condizionali e l'operatore `-d`. Di seguito è presente un esempio semplice che dimostra come effettuare questo controllo.

```bash
if [ -d "/percorso/alla/directory" ]; then
    echo "La directory esiste."
else
    echo "La directory non esiste."
fi
```

Esempio di output (se la directory esiste):
```
La directory esiste.
```

Esempio di output (se la directory non esiste):
```
La directory non esiste.
```

Per script più complessi, è comune combinare il controllo con altre operazioni, come la creazione della directory se non esiste:

```bash
DIR="/percorso/alla/directory"
if [ -d "$DIR" ]; then
    echo "$DIR esiste."
else
    echo "$DIR non esiste. Creazione in corso..."
    mkdir -p "$DIR"
    echo "$DIR creata."
fi
```

Esempio di output (se la directory non esiste e poi viene creata):
```
/percorso/alla/directory non esiste. Creazione in corso...
/percorso/alla/directory creata.
```

Sebbene Bash stesso fornisca strumenti robusti per tali controlli, non esistono librerie terze parti popolari specificamente per questo compito, poiché i comandi nativi di Bash sono completamente capaci ed efficienti per la validazione della presenza di directory.
