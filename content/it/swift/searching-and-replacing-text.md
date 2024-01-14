---
title:                "Swift: Cercare e sostituire testo"
simple_title:         "Cercare e sostituire testo"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori si trovano spesso a dover sostituire del testo all'interno del proprio codice. Questo può essere dovuto a diverse esigenze, come correggere un errore di ortografia o cambiare il nome di una variabile. Indipendentemente dalla ragione, saper cercare e sostituire il testo in modo efficiente è una competenza importante per ogni sviluppatore.

## Come fare

La ricerca e la sostituzione del testo in Swift è un'operazione semplice ma potente, grazie all'utilizzo dei metodi `replacingOccurrences(of:with:)` e `replacingOccurrences(of:with:options:range:)`. Supponiamo ad esempio di avere una stringa contenente il testo "Ciao unicorni!", e vogliamo sostituire la parola "unicorni" con "leoni". Possiamo farlo in questo modo:

```Swift
var stringa = "Ciao unicorni!"
stringa = stringa.replacingOccurrences(of: "unicorni", with: "leoni")
print(stringa) // Output: Ciao leoni!
```

In questo esempio, abbiamo semplicemente chiamato il metodo `replacingOccurrences` sulla nostra stringa, specificando la parola da cercare e quella con cui sostituirla. Il metodo restituisce una nuova stringa con le sostituzioni effettuate, che abbiamo assegnato nuovamente alla nostra variabile `stringa`.

Possiamo anche specificare delle opzioni aggiuntive, come ad esempio la distinzione tra maiuscole e minuscole o la ricerca solo all'interno di una determinata porzione di testo.

## Approfondimento

La sostituzione del testo in Swift utilizza le espressioni regolari, che sono delle potenti stringhe di ricerca che permettono di identificare pattern specifici. Ciò significa che possiamo effettuare delle sostituzioni basate su pattern, anziché cercare una stringa precisa. Ad esempio, se vogliamo sostituire tutte le lettere "e" seguite da una vocale con la lettera "è", possiamo farlo in questo modo:

```Swift
var stringa = "Amo la mela"
stringa = stringa.replacingOccurrences(of: "e[aeiou]", with: "è", options: .regularExpression)
print(stringa) // Output: Amò la mèla
```

In questo esempio, abbiamo utilizzato la stringa di ricerca "e[aeiou]", che significa "e seguito da una qualunque vocale". Abbiamo anche specificato l'opzione `regularExpression` per indicare a Swift di utilizzare le espressioni regolari per la ricerca del testo.

## Vedi anche
- [La guida ufficiale di Swift per la ricerca e la sostituzione del testo](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID103)
- [Una guida completa alle espressioni regolari in Swift](https://www.raywenderlich.com/862013-regular-expressions-in-swift-tutorial-getting-started)
- [Come sostituire il testo in un file con Swift](https://medium.com/@JohnSundell/replacing-strings-in-files-using-swift-scripting-518df19a1b8c)