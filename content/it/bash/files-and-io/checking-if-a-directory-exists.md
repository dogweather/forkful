---
aliases:
- /it/bash/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:41.913627-07:00
description: "Nella programmazione Bash, verificare se una directory esiste \xE8 un\
  \ meccanismo di controllo essenziale usato per verificare la presenza di una directory\u2026"
lastmod: 2024-02-18 23:08:56.066122
model: gpt-4-0125-preview
summary: "Nella programmazione Bash, verificare se una directory esiste \xE8 un meccanismo\
  \ di controllo essenziale usato per verificare la presenza di una directory\u2026"
title: Verifica se una directory esiste
---

{{< edit_this_page >}}

## Cos'è & Perché?

Nella programmazione Bash, verificare se una directory esiste è un meccanismo di controllo essenziale usato per verificare la presenza di una directory prima di eseguire operazioni sui file. Questo controllo è cruciale per evitare errori come tentare di accedere o modificare directory che non esistono, garantendo un'esecuzione dello script più fluida e prevedibile.

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
