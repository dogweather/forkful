---
title:                "Rimuovere le virgolette da una stringa"
date:                  2024-01-26T03:39:02.609319-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rimuovere le virgolette da una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Rimuovere le virgolette da una stringa significa togliere quegli strati extra – i segni di virgolettatura – dai tuoi dati di testo. I programmatori fanno ciò per sanificare l'input, preparare le stringhe per l'elaborazione, o semplicemente per mantenere le cose ordinate e consistenti nelle loro applicazioni. Alla fine si tratta di ottenere dati puliti e utilizzabili.

## Come fare:
Strippare le virgolette in Gleam è semplice. Possiamo usare il pattern matching o le funzioni predefinite per le stringhe. Ecco un esempio rapido per illustrare:

```gleam
pub fn remove_quotes(text: String) -> String {
  let without_quotes = string.trim(text, "\"")
  without_quotes
}

pub fn main() {
  let text_with_quotes = "\"Ciao, Mondo!\""
  let cleaned_text = remove_quotes(text_with_quotes)
  io.println(cleaned_text)
}
```

Output di esempio:
```
Ciao, Mondo!
```

## Approfondimento
Storicamente, occuparsi delle virgolette nelle stringhe è stato un compito comune nel processing di testo e nei linguaggi di scripting. Dato che la natura delle stringhe è spesso quella di essere input dell'utente o lette da file, possono arrivare con virgolette che necessitano di essere rimosse per vari motivi, come l'inserimento in un database o la formattazione.

In Gleam, usiamo la funzione `string.trim` per rasare via le virgolette. Ci sono alternative! Potremmo ciclare attraverso la stringa o applicare espressioni regolari, ma `string.trim` è il tuo strumento pratico per il lavoro a causa della sua brevità e performance.

Se ci immergiamo nei dettagli dell'implementazione, `string.trim` funziona rimuovendo i caratteri dall'inizio e dalla fine della stringa che corrispondono al modello fornito. Quindi se hai virgolette agli estremi della tua stringa, vengono tranciate via in un colpo solo. Tieni presente che rimuove le virgolette solo se sono ai bordi; le virgolette posizionate comodamente nel mezzo del tuo testo rimarranno al loro posto.

## Vedi Anche
Per le menti curiose là fuori che vogliono esplorare di più:
- [Documentazione del modulo String di Gleam](https://gleam.run/stdlib/string/)
- [Ulteriori informazioni sul pattern matching in Gleam](https://gleam.run/book/tour/pattern-matching)
- Discussioni sul processing di testo nella programmazione su [Stack Overflow](https://stackoverflow.com/questions/tagged/text-processing)