---
title:                "Avvio di un nuovo progetto"
aliases:
- /it/clojure/starting-a-new-project.md
date:                  2024-01-20T18:03:04.595548-07:00
model:                 gpt-4-1106-preview
simple_title:         "Avvio di un nuovo progetto"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Avviare un nuovo progetto significa configurare l'ambiente di sviluppo per piazzare solidamente le basi del software. I programmatori fanno questo per garantire che partano con le migliori pratiche, abbiano una struttura organizzata e possano integrare strumenti che aumentino l'efficienza dello sviluppo.

## How to: (Come fare:)
Clojure offre uno strumento integrato,`lein`, per iniziare nuovi progetti. Ecco come si usa:

```Clojure
;; Installa Leiningen seguendo le istruzioni su https://leiningen.org/
;; Poi, avvia un nuovo progetto con:
lein new app il-mio-nuovo-progetto

;; Hai creato un nuovo progetto Clojure! La struttura sarà simile a questa:
;.
;├── project.clj
;├── README.md
;├── resources
;├── src
;│   └── il_mio_nuovo_progetto
;│       └── core.clj
;└── test
;    └── il_mio_nuovo_progetto
;        └── core_test.clj

;; Per eseguire il progetto:
lein run

;; Per eseguire i test:
lein test
```

Il risultato del comando `lein run` sarà il classico "Hello, World!" se non hai cambiato il contenuto di `core.clj`.

## Deep Dive (Approfondimento)
`lein` è abbreviazione di Leiningen, uno strumento di automazione build pensato per Clojure. Prende il nome dal protagonista della storia "Leiningen Versus the Ants". Prima di Leiningen, i Clojuristi spesso usavano strumenti Java come Maven o Ant, ma questi non erano ottimizzati per Clojure. Leiningen offre templates e plugin, rendendo facile scalare il progetto. Alternativamente, c'è `clj` e `deps.edn` introdotti di recente per gestire dipendenze in un modo più semplice.

## See Also (Vedi Anche)
- [Leiningen Home Page](https://leiningen.org/)
- [Clojure Getting Started Guide](https://clojure.org/guides/getting_started)
- [ClojureScript, per Clojure su browser](https://clojurescript.org/)
- [Practicalli Clojure, risorse per iniziare ed approfondire](https://practical.li/clojure/)
