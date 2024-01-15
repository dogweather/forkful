---
title:                "Lettura di un file di testo."
html_title:           "Ruby: Lettura di un file di testo."
simple_title:         "Lettura di un file di testo."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Perché

Ci sono molte ragioni per cui si potrebbe desiderare di leggere un file di testo in Ruby. Forse si sta creando un programma che deve elaborare dati da un file esterno, o forse si sta cercando di analizzare il contenuto di un file per ottenere informazioni utili.

# Come fare

Per leggere un file di testo con Ruby è necessario seguire i seguenti passaggi:

1. Aprire il file utilizzando il metodo `File.open()`, specificando il percorso del file come argomento.
2. Utilizzare il metodo `read()` per leggere il contenuto del file e salvarlo in una variabile.
3. Chiudere il file utilizzando il metodo `close()`.
4. Eseguire delle operazioni sul contenuto del file.

Ad esempio, se vogliamo stampare il contenuto del file su schermo, possiamo utilizzare il seguente codice:

```Ruby
file = File.open("percorso_del_file")
contents = file.read()
file.close()

puts contents
```

Il risultato sarà il seguente:

```
Questo è un esempio di contenuto di un file di testo.
```

# Approfondimento

Oltre al metodo `read()`, esistono altre opzioni per leggere un file di testo in Ruby, come ad esempio i metodi `readlines()` e `each_line()`. Questi metodi permettono di leggere il contenuto del file riga per riga anziché in un unico blocco.

Inoltre, è possibile specificare diverse opzioni durante l'apertura del file, come ad esempio il modo in cui il testo è codificato o se si desidera leggere solo una parte del file.

# Vedi anche

- [Ruby's File Class](https://ruby-doc.org/core-3.0.0/File.html)
- [Ruby File Open()](https://ruby-doc.org/core-3.0.0/File.html#method-c-open)
- [Ruby File Read()](https://ruby-doc.org/core-3.0.0/IO.html#method-i-read)