---
title:                "Verifica se una directory esiste"
aliases:
- it/fish-shell/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:12.790200-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verifica se una directory esiste"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?
Verificare se una directory esiste in Fish Shell permette agli script di prendere decisioni basate sulla presenza o assenza di strutture di directory, consentendo compiti come operazioni su file condizionali, registrazione o configurazione dell'ambiente. Questa tecnica è fondamentale per scrivere script robusti che interagiscono con il filesystem in modo prevedibile.

## Come fare:
Fish Shell utilizza il comando `test` per verificare tipi di file e caratteristiche, inclusa la verifica se un target è una directory. Ecco uno schema di base per controllare se una directory esiste:

```fish
if test -d /percorso/alla/dir
    echo "La directory esiste"
else
    echo "La directory non esiste"
end
```
Output Esempio:
```
La directory esiste
```

Per operazioni su file e directory più semplificate, si potrebbe ricorrere a strumenti esterni come `fd`, anche se è più comunemente usato per trovare file e directory piuttosto che semplicemente per verificare l'esistenza. Tuttavia, combinandolo con lo scripting di Fish può fornire risultati pratici:

```fish
set dir "/percorso/da/cercare"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "La directory esiste"
else
    echo "La directory non esiste"
end
```

Questo esempio con `fd` cerca la directory a una profondità specificata, e `grep` verifica la corrispondenza, rendendolo versatile per controlli sfumati. Tuttavia, per lo scopo diretto di verificare l'esistenza, attenersi al `test` integrato di Fish è sia efficiente che semplice.
