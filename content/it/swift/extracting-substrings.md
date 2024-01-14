---
title:                "Swift: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

# Perché

La funzione di estrazione delle sottostringhe è molto utile nel processo di manipolazione delle stringhe in Swift. Ti permette di estrarre una parte specifica di una stringa, rendendo più facile e veloce l'accesso ai dati di cui hai bisogno.

# Come utilizzarla

Per utilizzare la funzione di estrazione delle sottostringhe in Swift, segui questi semplici passaggi:

1. Definisci la tua stringa originale:
```
let stringa = "Benvenuti alla nostra lezione di programmazione in Swift!"
```

2. Utilizza il metodo `index` per selezionare gli indici di inizio e fine della tua sottostringa:
```
let inizio = stringa.index(stringa.startIndex, offsetBy: 11) // 11 è l'indice del carattere "n" di "nostra"
let fine = stringa.index(stringa.endIndex, offsetBy: -8) // -8 fa riferimento agli ultimi 8 caratteri della stringa
```

3. Utilizza il metodo `substring` per estrarre la sottostringa:
```
let sottostringa = stringa.substring(with: inizio..<fine)
```

Nel nostro esempio, la sottostringa estratta sarebbe "nostra lezione di programmazione".

# Approfondimento

La funzione di estrazione delle sottostringhe in Swift è molto flessibile e ti consente di utilizzare vari metodi per selezionare gli indici di inizio e fine della tua sottostringa. Puoi utilizzare il metodo `prefix` per estrarre una sottostringa dalla posizione iniziale fino ad un certo numero di caratteri, o il metodo `suffix` per estrarre una sottostringa dalla posizione finale fino ad un certo numero di caratteri.

Inoltre, puoi anche combinare il metodo `substring` con altri metodi di manipolazione delle stringhe in Swift, come `replacingOccurrences` per sostituire determinati caratteri nella sottostringa estratta.

# Vedi anche

- [Documentazione su stringhe in Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Esempi di codice per l'estrazione delle sottostringhe in Swift](https://www.hackingwithswift.com/syntax-guide/strings-and-characters/extracting-parts-of-a-string)