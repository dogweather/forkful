---
title:    "Clojure: Convertire una stringa in minuscolo"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo è un'operazione utile quando si lavora con testi e si desidera uniformare il formato delle parole. Ciò può essere utile, ad esempio, quando si esegue il confronto tra due stringhe o si vuole essere certi che il testo inserito dall'utente sia sempre in minuscolo.

## Come Fare

Per convertire una stringa in minuscolo in Clojure, è possibile utilizzare la funzione `lower-case` del namespace `clojure.string`. Ecco un esempio di codice:

```Clojure
(require '[clojure.string :as str])

(str/lower-case "CIAO AMICI") 
```

Questo codice produrrà l'output "ciao amici". La funzione `lower-case` prende come input una stringa e restituisce la stessa stringa con tutti i caratteri convertiti in minuscolo.

## Approfondimento

La funzione `lower-case` utilizza l'Unicode Standard per gestire i diversi tipi di carattere. Ciò significa che in alcuni casi può essere imprevedibile la conversione di determinati caratteri speciali o accentati. Per una gestione più controllata delle conversioni, è possibile utilizzare la funzione `lower-case-locales` che prende in input una locale e una stringa e restituisce la stringa con i caratteri convertiti in minuscolo secondo le regole della locale specificata.

## Vedi Anche

- [Documentation for clojure.string](https://clojuredocs.org/clojure.string/lower-case)
- [Unicode Standard](https://www.unicode.org)
- [Locale-specific mapping for lower-case](https://docs.oracle.com/javase/7/docs/api/java/text/Normalizer.html)