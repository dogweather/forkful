---
title:    "Ruby: Converione di una stringa in minuscolo"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Molte volte durante la scrittura di codice, potresti avere la necessità di convertire una stringa in lower case. Questo è utile nei casi in cui devi effettuare una comparazione tra due stringhe senza considerare la differenza tra maiuscole e minuscole.

## Come fare

Per convertire una stringa in lower case in Ruby, puoi utilizzare il metodo `downcase`.

```
Ruby stringa = "Ciao a tutti!"
puts stringa.downcase
```

Output: `ciao a tutti!`

Puoi anche assegnare il risultato alla stessa variabile se vuoi modificare permanentemente la stringa.

```
stringa = stringa.downcase
puts stringa
```

Output: `ciao a tutti!`

Se invece vuoi solo fare una comparazione tra due stringhe senza alterare la stringa originale, puoi utilizzare il metodo `casecmp`.

```
stringa1 = "Ciao a tutti!"
stringa2 = "ciao a tutti!"
puts stringa1.casecmp(stringa2) == 0
```

Output: `true`

## Approfondimento

Il metodo `downcase` utilizza le regole di conversione specifiche di ogni lingua per convertire le lettere maiuscole in lettere minuscole. Ad esempio, il carattere "I" in una stringa in lingua inglese verrà convertito in "i", mentre in una stringa in lingua tedesca verrà convertito in "ı".

Puoi anche utilizzare il metodo `downcase!` per modificare la stringa originale senza dover assegnare nuovamente il risultato alla stessa variabile.

```
stringa = "Buongiorno a tutti!"
stringa.downcase!
puts stringa
```

Output: `buongiorno a tutti!`

## Vedi anche
- [Ruby string class documentation](https://ruby-doc.org/core-3.0.1/String.html)
- [Ruby string methods](https://www.rubyguides.com/ruby-string-methods/)