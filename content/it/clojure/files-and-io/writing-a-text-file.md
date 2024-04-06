---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:31.547591-07:00
description: "Come fare: La funzione `spit` \xE8 il modo pi\xF9 semplice per scrivere\
  \ testo in un file in Clojure. Richiede due argomenti: il percorso del file e la\
  \ stringa\u2026"
lastmod: '2024-03-13T22:44:43.060510-06:00'
model: gpt-4-0125-preview
summary: "La funzione `spit` \xE8 il modo pi\xF9 semplice per scrivere testo in un\
  \ file in Clojure."
title: Scrivere un file di testo
weight: 24
---

## Come fare:


### Scrivere testo in un file usando le funzioni integrate di Clojure
La funzione `spit` è il modo più semplice per scrivere testo in un file in Clojure. Richiede due argomenti: il percorso del file e la stringa da scrivere. Se il file non esiste, `spit` lo creerà. Se esiste, `spit` lo sovrascriverà.

```clojure
(spit "esempio.txt" "Ciao, mondo!")
```

Per aggiungere testo a un file esistente, puoi usare la funzione `spit` con l'opzione `:append`.

```clojure
(spit "esempio.txt" "\nAggiungiamo questa nuova riga." :append true)
```

Dopo aver eseguito questi snippet, "esempio.txt" conterrà:

```
Ciao, mondo!
Aggiungiamo questa nuova riga.
```

### Utilizzare librerie di terze parti
Sebbene le capacità integrate di Clojure siano spesso sufficienti, la comunità ha sviluppato robuste librerie per compiti più complessi o specifici. Per l'I/O su file, una libreria popolare è `clojure.java.io`, che offre un approccio più simile a Java per la gestione dei file.

Per usare `clojure.java.io` per scrivere in un file, prima devi importarla:

```clojure
(require '[clojure.java.io :as io])
```

Poi, puoi utilizzare la funzione `writer` per ottenere un oggetto writer, e la funzione `spit` (o altre come `print`, `println`) per scrivere nel file:

```clojure
(with-open [w (io/writer "esempio_con_io.txt")]
  (.write w "Questo è scritto usando clojure.java.io"))
```

Questo creerà (o sovrascriverà se già esiste) "esempio_con_io.txt" con il testo:

```
Questo è scritto usando clojure.java.io
```

Ricorda: `with-open` garantisce che il file sia correttamente chiuso dopo la scrittura, evitando possibili perdite di risorse.
