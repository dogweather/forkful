---
date: 2024-01-27 20:33:17.284888-07:00
description: "Generare numeri casuali \xE8 un compito fondamentale nella programmazione,\
  \ utilizzato per tutto, dalla campionatura dei dati allo sviluppo di giochi. In\
  \ Fish\u2026"
lastmod: '2024-03-13T22:44:43.852245-06:00'
model: gpt-4-0125-preview
summary: "Generare numeri casuali \xE8 un compito fondamentale nella programmazione,\
  \ utilizzato per tutto, dalla campionatura dei dati allo sviluppo di giochi."
title: Generazione di numeri casuali
weight: 12
---

## Come fare:
Generare un numero casuale in Fish può essere semplice, utilizzando la combinazione di strumenti di sistema e capacità del shell. Di seguito sono riportati alcuni esempi che dimostrano come generare numeri casuali entro intervalli specificati.

**Generare un numero casuale tra 0 e 100:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**Esempio di Output:**
```fish
42
```

**Generare un numero casuale tra due numeri qualsiasi, diciamo 50 e 150:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**Esempio di Output:**
```fish
103
```

**Usare random per mescolare un elenco:**

Potresti anche voler mescolare casualmente gli elementi in un elenco. Ecco come puoi farlo:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**Esempio di Output:**
```fish
C
A
E
D
B
```

Si prega di notare, l'output varierà ogni volta che esegui questi comandi a causa della natura della casualità.

## Approfondimento
La funzione `random` di Fish Shell fornisce un'interfaccia facile da usare per generare numeri pseudo-casuali. Internamente, si avvale di strumenti di generazione di numeri casuali a livello di sistema, offrendo un modo portatile per introdurre casualità nei vostri script. Tuttavia, è essenziale ricordare che la casualità fornita da `random` è sufficiente per la maggior parte dei compiti di scripting, ma potrebbe non soddisfare i requisiti di sicurezza crittografica per le applicazioni che necessitano di un grado di imprevedibilità più elevato.

Per contesti di sicurezza ad alto rischio, considerare l'uso di strumenti dedicati o librerie di programmazione progettate per scopi crittografici, che offrono garanzie di casualità più forti. Tuttavia, per script generici e applicazioni dove gli standard di sicurezza più elevati per la casualità non sono una necessità, la funzione `random` di Fish Shell offre una soluzione conveniente ed efficace.
