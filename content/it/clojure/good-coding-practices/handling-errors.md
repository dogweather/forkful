---
date: 2024-01-26 00:50:41.678251-07:00
description: "La gestione degli errori riguarda il controllo dell'inaspettato nei\
  \ programmi, proprio come un buttafuori che si occupa dei guastafeste. Ai programmatori\u2026"
lastmod: 2024-02-19 22:05:02.158607
model: gpt-4-1106-preview
summary: "La gestione degli errori riguarda il controllo dell'inaspettato nei programmi,\
  \ proprio come un buttafuori che si occupa dei guastafeste. Ai programmatori\u2026"
title: Gestione degli errori
---

{{< edit_this_page >}}

## Cosa & Perché?
La gestione degli errori riguarda il controllo dell'inaspettato nei programmi, proprio come un buttafuori che si occupa dei guastafeste. Ai programmatori piace che tutto fili liscio; la gestione degli errori aiuta a tenere i problemi sotto controllo, assicurandosi che il loro codice non inciampi e cada di fronte all'imprevisto.

## Come fare:
Clojure, come i suoi antenati Lisp, si affida alle eccezioni per la gestione degli errori. Ecco come mostrare di cosa sei capace quando le cose vanno male.

Lanciare un'eccezione è semplice:
```Clojure
(throw (Exception. "Oops! Qualcosa è andato storto."))
```

Catturare un'eccezione, lo farai spesso:
```Clojure
(try
  ;; codice a rischio
  (/ 1 0)
  (catch ArithmeticException e
    (println "Non si può dividere per zero!"))
  ;; il blocco finally viene eseguito comunque
  (finally 
    (println "Il codice di pulizia va qui.")))
```
Esempio di output per il blocco catch sopra:
```
Non si può dividere per zero!
Il codice di pulizia va qui.
```

Utilizzando `ex-info` e `ex-data` per un contesto più ricco riguardo alle eccezioni:
```Clojure
(try
  ;; causando un'eccezione personalizzata
  (throw (ex-info "Errore personalizzato" {:type :custom-failure}))
  (catch Exception e
    ;; estrarre i dati dalla nostra eccezione personalizzata
    (println (ex-data e))))
```
Esempio di output:
```
{:type :custom-failure}
```

## Approfondimento
La storia della gestione degli errori in Clojure non è radicalmente diversa da altri Lisp o persino da Java (da cui eredita il meccanismo `try-catch`). È pragmatica; l'uso delle eccezioni è la via principale, proprio come in Java, ma Clojure offre un tocco funzionale con `ex-info` e `ex-data` per dati di errore più ricchi.

Le alternative per la gestione degli errori in Clojure includono l'uso di costrutti monadici, come la monade `either` da librerie come `cats`, o core.async per la propagazione di errori basata su canali. Tuttavia, questi sono più complessi e usati in scenari specifici.

Storicamente, la gestione degli errori nei linguaggi di programmazione si è evoluta dai semplici ritorni di stato ai meccanismi di gestione delle eccezioni più sofisticati dei linguaggi moderni. Clojure opta per la semplicità e un tocco di programmazione funzionale, miscelando il vecchio e il nuovo.

## Vedi Anche
- Guida di Clojure alle eccezioni: https://clojure.org/guides/exceptions
- Libreria "Cats" per approcci più funzionali: https://github.com/funcool/cats
- "Core.async" per la programmazione asincrona: https://github.com/clojure/core.async
