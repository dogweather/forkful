---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:12.790200-07:00
description: "Come fare: Fish Shell utilizza il comando `test` per verificare tipi\
  \ di file e caratteristiche, inclusa la verifica se un target \xE8 una directory.\
  \ Ecco uno\u2026"
lastmod: '2024-03-13T22:44:43.873812-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell utilizza il comando `test` per verificare tipi di file e caratteristiche,\
  \ inclusa la verifica se un target \xE8 una directory."
title: Verifica se una directory esiste
weight: 20
---

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
