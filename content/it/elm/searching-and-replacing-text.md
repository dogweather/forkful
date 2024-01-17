---
title:                "Cercare e sostituire testo"
html_title:           "Elm: Cercare e sostituire testo"
simple_title:         "Cercare e sostituire testo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Cosa & Perché?

La ricerca e la sostituzione di testo sono un processo comune nella programmazione. Consiste nel cercare una determinata sequenza di caratteri in un testo e sostituirla con un'altra. I programmatori spesso utilizzano questo processo per correggere errori di ortografia, modificare nomi di variabili o funzioni, o per effettuare altre modifiche su larga scala.

Come fare:

Elm offre diverse funzioni per la ricerca e la sostituzione di testo. Ad esempio, la funzione "replace" prende tre argomenti: la sequenza di testo da cercare, la sequenza di testo con cui sostituirla e il testo in cui cercare la sequenza. Esempio:

```Elm
replace "s" "z" "hello" -- risultato: "hezzo"
```

Ci sono anche altre opzioni disponibili come le funzioni "replaceFirst" e "replaceLast", che forniscono rispettivamente la sostituzione solo del primo o dell'ultimo match trovato.

Un altro modo per effettuare la ricerca e la sostituzione di testo in Elm è utilizzare le espressioni regolari. Le espressioni regolari sono modelli di ricerca di testo più avanzati che consentono di effettuare ricerche più precise e di sostituire grandi quantità di testo con una singola linea di codice. Di seguito un esempio di espressione regolare che sostituisce tutte le vocali nella stringa con un punto esclamativo:

```Elm
import Regex
import String.Extra

Regex.replace (Regex.regex "[aeiou]") (\\_ -> "!") "hello world" -- risultato: "h!ll! w!rld"
```

Un ulteriore approccio è quello di utilizzare la funzione "replaceWith" che consente di specificare una funzione personalizzata per la sostituzione di ogni corrispondenza trovata. Ad esempio, la seguente funzione sostituisce la lettera "a" con la sua versione maiuscola:

```Elm
import Html

replaceWith (\\c -> if c == 'a' then 'A' else c) "hello world" -- risultato: "hello world"
```

Approfondimento:

La sostituzione di testo è stata resa molto più semplice e conveniente grazie all'uso di espressioni regolari in Elm. In passato, i programmatori dovevano utilizzare metodi più complicati per effettuare ricerche e sostituzioni di testo. Alcuni linguaggi di programmazione offrono funzioni simili a quelle di Elm, ma spesso richiedono molte più linee di codice per raggiungere lo stesso risultato.

Vedi anche:

Per imparare di più sulle espressioni regolari in Elm, consulta la documentazione ufficiale di Elm o esplora tutorial e guide online. Inoltre, puoi anche esplorare altre funzioni per la ricerca e la sostituzione di testo in Elm come "search" e "replaceAll". Buon divertimento!