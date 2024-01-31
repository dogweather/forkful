---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"

category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa significa rendere maiuscola la prima lettera di ogni parola. Lo facciamo per questioni di formattazione, nomi propri, titoli o per rendere il testo più leggibile e professionale.

## How to:
```Javascript
function capitalize(str) {
  return str.replace(/^(.)|\s+(.)/g, c => c.toUpperCase());
}

console.log(capitalize('ciao mondo, siamo qui per imparare!'));
// Output: Ciao Mondo, Siamo Qui Per Imparare!
```

## Deep Dive
La capitalizzazione delle stringhe in JavaScript non ha una funzione nativa, dunque usiamo il metodo `replace()` con un'espressione regolare. Questo approccio è ormai consolidato nella comunità di sviluppatori. Oltre alla soluzione mostrata, esistono alternative come loop manuali o l'utilizzo di librerie esterne. Tuttavia, la `replace()` con regex è efficiente e largamente adottata per la sua brevità e chiarezza nel codice.

In contesti più complessi, come la gestione di casi speciali (ad esempio acronimi o particolari regole di capitalizzazione), potresti dover implementare una logica aggiuntiva o scegliere una libreria che già gestisce tali eventualità.

## See Also
- MDN Web Docs sulla funzione `replace()`: [MDN - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Espressioni regolari in JavaScript: [MDN - RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Una libreria per la manipolazione delle stringhe che supporta diverse operazioni di capitalizzazione: [Lodash](https://lodash.com/docs/#capitalize)
