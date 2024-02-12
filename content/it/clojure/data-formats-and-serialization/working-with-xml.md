---
title:                "Lavorare con XML"
aliases:
- /it/clojure/working-with-xml/
date:                  2024-01-26T04:29:04.694659-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-xml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
XML è un linguaggio di marcatura per codificare documenti in un modo che sia leggibile sia dall'uomo che dalla macchina. È fondamentale nei servizi web, nei file di configurazione e nello scambio di dati perché trasporta i dati in un formato strutturato e gerarchico.

## Come fare:
Clojure offre la libreria `clojure.data.xml` per il parsing e l'emissione di XML. Prima, analizziamo un po' di XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; Analizza la stringa XML
  (println parsed))
```
Output:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Per emettere XML da strutture Clojure:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
Output:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Approfondimento
XML è presente da un bel po', avviato alla fine degli anni '90 come un sottogruppo semplificato di SGML, destinato ai dati web. Il suo uso è esploso con tecnologie come SOAP e XHTML ma ha ottenuto un po' di concorrenza da JSON, che è preferito per la sua leggerezza e semplicità.

L'approccio di Clojure all'XML lo mantiene funzionale e centrato sui dati, rimanendo fedele all'etos del linguaggio. `clojure.data.xml` è semplicemente un'opzione; hai `clojure.xml` per le esigenze di base e, per l'interoperabilità con Java, puoi affidarti a pesi massimi come JAXB o DOM4J.

Tieni presente che le prestazioni e il sovraccarico di memoria quando si trattano documenti XML molto grandi possono essere onerosi. I parser in streaming come StAX possono aiutare, ma dovrai passare al mondo Java per utilizzarli.

## Vedi Anche
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
