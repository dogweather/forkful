---
title:    "Ruby: Maiuscolo di una stringa"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa può essere un'operazione utile se si lavora su dati testuali e si vuole uniformare il formato delle parole. Ad esempio, se si hanno nomi di utenti inseriti in modo diverso, capitalizzare la stringa può aiutare a renderli coerenti e più leggibili.

## Come Fare

Per capitalizzare una stringa in Ruby, si può utilizzare il metodo `capitalize`. Questo metodo restituisce una nuova stringa con la prima lettera maiuscola e tutte le altre lettere minuscole.

```Ruby
stringa = "ciao mondo!"
puts stringa.capitalize
```

Output:
```
Ciao mondo!
```

Si possono anche capitalizzare tutte le parole in una stringa utilizzando il metodo `titleize`.

```Ruby
stringa = "questo è un titolo"
puts stringa.titleize
```

Output:
```
Questo È Un Titolo
```

## Approfondimento

Il metodo `capitalize` utilizza delle regole specifiche per capitalizzare la stringa. Ad esempio, se una parola inizia con una lettera accentata, verrà considerata come una lettera diversa e non verrà maiuscola. Inoltre, se una parola inizia con un carattere speciale, verrà considerata come parte della parola e verrà capitalizzata.

Esistono anche altri metodi e gemme disponibili per la capitalizzazione di stringhe in modo più personalizzato, come ad esempio il linguaggio YAML o la gemma "#{ActiveSupport}::Inflector".

## Vedi Anche

- [Documentazione ufficiale sul metodo `capitalize`](https://ruby-doc.org/core-2.7.1/String.html#method-i-capitalize)
- [Documentazione ufficiale sul metodo `titleize`](https://apidock.com/rails/String/titleize)
- [Guida su come utilizzare #{ActiveSupport}::Inflector](https://masayoshi634.hatenablog.com/entry/2019/06/19/213123)