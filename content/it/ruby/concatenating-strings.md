---
title:    "Ruby: Concatenazione di stringhe"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Perché concatenare le stringhe in Ruby

Se sei un programmatore Ruby, probabilmente hai già familiarità con il concetto di concatenazione delle stringhe. Ma per coloro che sono nuovi al linguaggio, ecco perché è importante imparare a concatenare le stringhe in Ruby.

La concatenazione delle stringhe è utile quando si vuole combinare una serie di stringhe per creare una stringa più lunga. Può essere utile per creare output personalizzato, stringhe di ricerca o per formattare i dati in un modo specifico.

## Come concatenare le stringhe in Ruby

In Ruby, esistono diverse opzioni per concatenare le stringhe. La più semplice è utilizzare l'operatore "+" per unire due stringhe, come nel seguente esempio:

```Ruby
"Benvenuto" + "in Ruby!"
```

L'output di questo codice sarebbe:

```
Benvenuto in Ruby!
```

Un'altra opzione è utilizzare il metodo "concat" per aggiungere una stringa alla fine di un'altra. Ad esempio:

```Ruby
"Come" .concat ("stai?")
```

Produrrà lo stesso output di prima:

```
Come stai?
```

Infine, è possibile utilizzare l'operatore "<<", anche noto come l'operatore di appendice, per concatenare le stringhe. Ecco un esempio:

```Ruby
"Buon" << "giorno"
```

L'output sarebbe:

```
Buon giorno
```

## Approfondimento sulla concatenazione delle stringhe

Mentre tutti questi metodi funzionano per concatenare le stringhe, l'operatore "<<" è di solito la scelta migliore per motivi di efficienza. Quando si utilizza l'operatore di concatenazione, Ruby crea una nuova stringa ogni volta che si aggiunge una stringa. Ma con l'operatore "<<" , viene semplicemente aggiunta una stringa alla fine della stringa esistente, senza la necessità di creare una nuova.

Inoltre, è possibile concatenare più di due stringhe alla volta utilizzando l'operatore "<<", come nel seguente esempio:

```Ruby
"Buon" << "pomeriggio" << "a tutti!"
```

Questo produrrà l'output:

```
Buon pomeriggio a tutti!
```

Inoltre, è importante sapere che è possibile concatenare stringhe di diversi tipi di variabili, come numeri o booleani, utilizzando il metodo "to_s" per convertirli in stringhe prima di concatenarli.

## Vedi anche

* [Documentazione ufficiale di Ruby sulla concatenazione delle stringhe](https://www.ruby-lang.org/it/documentation/)
* [Un tutorial su Ruby dedicato alla concatenazione delle stringhe](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
* [Un esempio pratico di concatenazione delle stringhe in Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-string-int-methods/cheatsheet)