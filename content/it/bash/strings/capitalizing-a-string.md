---
aliases:
- /it/bash/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:52.471856-07:00
description: "Capitalizzare una stringa in Bash comporta la trasformazione del primo\
  \ carattere della stringa in maiuscolo lasciando inalterata il resto della stringa.\u2026"
lastmod: 2024-02-18 23:08:56.035218
model: gpt-4-0125-preview
summary: "Capitalizzare una stringa in Bash comporta la trasformazione del primo carattere\
  \ della stringa in maiuscolo lasciando inalterata il resto della stringa.\u2026"
title: Capitalizzare una stringa
---

{{< edit_this_page >}}

## Cosa e Perché?
Capitalizzare una stringa in Bash comporta la trasformazione del primo carattere della stringa in maiuscolo lasciando inalterata il resto della stringa. Questa tecnica è comunemente utilizzata per formattare l'output o per conformarsi a convenzioni di codifica che richiedono che certe stringhe inizino con una lettera maiuscola per leggibilità o preferenze stilistiche.

## Come fare:

Bash non ha una funzione integrata specificamente per capitalizzare le stringhe, ma puoi realizzare questo compito utilizzando l'espansione dei parametri o strumenti esterni come `awk`. Ecco alcuni modi per capitalizzare una stringa in Bash:

**Utilizzando l'Espansione dei Parametri:**

Questo metodo manipola direttamente la stringa nella shell.

```bash
str="ciao mondo"
capitalized="${str^}"
echo "$capitalized"
```
Output:
```
Ciao mondo
```

**Utilizzando `awk`:**

`awk` è uno strumento potente per l'elaborazione di testo disponibile nella maggior parte dei sistemi operativi Unix-like, che può essere utilizzato per capitalizzare le stringhe.

```bash
str="ciao mondo"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
Output:
```
Ciao mondo
```

**Utilizzando `sed`:**

Per un approccio più tradizionale, `sed` può essere impiegato per capitalizzare la prima lettera di una stringa. Tuttavia, è un po' più complesso rispetto ai metodi precedenti.

```bash
str="ciao mondo"
echo "$str" | sed 's/./\u&/'
```
Output:
```
Ciao mondo
```

Questi frammenti dimostrano come capitalizzare la prima lettera di una stringa in Bash, evidenziando la flessibilità della programmazione della shell nella manipolazione del testo.
