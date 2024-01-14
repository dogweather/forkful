---
title:    "Ruby: Concatenazione di stringhe"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

In programmazione, la concatenazione di stringhe è un'operazione comune e utile per combinare diverse stringhe in una sola. Questo processo è spesso utilizzato quando si lavora con i dati, ad esempio per creare un messaggio personalizzato o per combinare informazioni di più variabili. Vediamo come farlo utilizzando Ruby!

## Come fare

Per concatenare le stringhe in Ruby, è possibile utilizzare l'operatore `+` o il metodo `.concat`. Ecco un esempio con entrambi i metodi:

```Ruby
stringa_1 = "Ciao"
stringa_2 = "mondo!"

# Utilizzando l'operatore +
nuova_stringa = stringa_1 + " " + stringa_2 
# Output: "Ciao mondo!"

# Utilizzando il metodo .concat
nuova_stringa = stringa_1.concat(" ", stringa_2)
# Output: "Ciao mondo!"
```

Come si può vedere dagli esempi, entrambi i metodi hanno prodotto lo stesso risultato. Tuttavia, è importante notare che l'operatore `+` crea una nuova stringa ogni volta che viene utilizzato, mentre il metodo `.concat` modifica la variabile esistente. Questo può avere un impatto sulle prestazioni del codice, quindi è importante scegliere il metodo più adatto alla situazione.

## Approfondimento

Oltre ai metodi sopra menzionati, è anche possibile concatenare stringhe utilizzando l'operatore di assegnazione `<<`, come mostrato nell'esempio seguente:

```Ruby
stringa_1 = "Ciao"
stringa_2 = "mondo!"

# Utilizzando l'operatore di assegnazione
stringa_1 << " " << stringa_2
# Output: "Ciao mondo!"
```

Questo metodo è particolarmente utile quando si lavora con stringhe dinamiche, come nel caso di cicli o loop. Inoltre, è possibile concatenare più di due stringhe utilizzando lo stesso operatore.

## Vedi anche
- [Concatenazione delle stringhe in Ruby](https://www.rubyguides.com/2015/03/ruby-string-concatenation/)
- [Documentazione ufficiale di Ruby su String](https://ruby-doc.org/core-2.7.1/String.html)
- [Concatenazione delle stringhe in altre lingue di programmazione](https://www.freecodecamp.org/news/how-to-concatenate-strings-in-different-programming-languages-1ae38b344c7b/)