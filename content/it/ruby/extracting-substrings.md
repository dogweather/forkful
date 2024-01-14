---
title:    "Ruby: Estrazione di sottostringhe"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe, o la rimozione di una parte specifica di una stringa, è un'operazione comune in programmazione. Può essere utile per manipolare e analizzare dati di testo o per creare output formattati in un modo specifico. In questo post esploreremo come eseguire questa operazione in Ruby.

## Come fare

Per estrarre una sottostringa da una stringa, possiamo utilizzare il metodo `slice`, che prende due argomenti: l'indice di inizio e la lunghezza della sottostringa da estrarre. Ad esempio, se vogliamo estrarre la sottostringa "world" dalla parola "hello world", possiamo farlo in questo modo:

```Ruby
testo = "hello world"
sottostringa = testo.slice(6, 5)
puts sottostringa #=> world
```

Possiamo anche utilizzare gli operatori di slicing per estrarre sottostringhe. Gli operatori sono `array[start, length]` e `array[start..end]`, dove `start` è l'indice di inizio e `end` è l'indice di fine della sottostringa da estrarre. Ad esempio, possiamo estrarre la sottostringa "hello" dalla parola "hello world" in questo modo:

```Ruby
testo = "hello world"
sottostringa = testo[0, 5]
puts sottostringa #=> hello
```

Possiamo anche combinare l'operatore di slicing con i metodi `split` e `join` per estrarre parti specifiche di una stringa complessa. Ad esempio, se abbiamo una stringa composta da nome, cognome e età separate da un carattere speciale, possiamo estrarre solo il nome in questo modo:

```Ruby
testo = "John-Doe-35"
nome = testo.split("-")[0]
puts nome #=> John
```

## Approfondimento

È importante notare che gli indici delle stringhe in Ruby partono da 0. Inoltre, gli operatori di slicing accettano valori negativi che indicano gli indici partendo dalla fine della stringa, come `-1` per l'ultimo carattere.

Inoltre, vale la pena menzionare che il metodo `slice` restituisce una nuova stringa, mentre gli operatori di slicing modificano la stringa originale. È importante prestare attenzione a questo quando si utilizzano questi metodi.

## Vedi anche

Per ulteriori informazioni su come estrarre sottostringhe in Ruby, puoi consultare i seguenti link:

- [Documentazione di Ruby sugli operatori di slicing](https://ruby-doc.org/core-2.6.3/String.html#method-i-5B)
- [Tutorial su come estrarre parti specifiche di una stringa in Ruby](https://www.rubyguides.com/2019/08/ruby-string-slice/)
- [Una panoramica di tutti i metodi disponibili per le stringhe in Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)

Grazie per aver letto questo post e speriamo ti sia stato di aiuto per comprendere meglio come estrarre sottostringhe in Ruby. Buon coding! 🚀