---
title:    "Clojure: Calcolare una data nel futuro o nel passato"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato è fondamentale per molti sviluppatori di software, in quanto permette di gestire in modo efficiente eventi futuri o passati all'interno dei loro programmi.

## Come fare

Per calcolare una data nel futuro o nel passato in Clojure, è necessario utilizzare la funzione `clj-time.core/plus` fornita dalla libreria `clj-time`. Questa funzione prende due parametri: la data di riferimento e un oggetto `org.joda.time.Period` che specifica di quanto tempo è necessario spostarsi.

Ad esempio, per ottenere la data di oggi nel formato DD/MM/YYYY è sufficiente eseguire il seguente codice:

```Clojure
(clj-time.coerce/to-date (clj-time.core/today))
```

Per ottenere la data di domani, è necessario aggiungere un giorno alla data di oggi:

```Clojure
(clj-time.coerce/to-date (clj-time.core/plus (clj-time.core/today) (clj-time.coerce/from-str "P1D")))
```

L'output di questo codice sarà la data di domani nel formato DD/MM/YYYY.

## Approfondimento

Per calcolare una data nel futuro o nel passato in modo più preciso, è possibile utilizzare gli oggetti `org.joda.time.Days`, `org.joda.time.Months` e `org.joda.time.Years` al posto di `org.joda.time.Period`. Questi oggetti consentono di specificare un numero preciso di giorni, mesi o anni da aggiungere o sottrarre alla data di riferimento.

Inoltre, la libreria `clj-time` offre anche la possibilità di gestire il fuso orario e il formato di output della data, fornendo maggiore flessibilità e precisione nei calcoli delle date.

## Vedi anche

- [Documentazione sulla libreria clj-time] (https://github.com/clj-time/clj-time)
- [Tutorial su come gestire le date in Clojure] (https://www.martiansoftware.com/clj-time/)
- [Esempi di codice sulla gestione delle date con Clojure] (https://gist.github.com/quangv/968125)