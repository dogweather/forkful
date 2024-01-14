---
title:                "Ruby: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché fare un controllo se una directory esiste in Ruby

Controllare se una directory esiste è un'operazione comune nei programmi Ruby, soprattutto quando si lavora con file e cartelle. Questo controllo è importante perché permette di evitare errori durante l'esecuzione del codice, come ad esempio cercare di accedere ad una directory inesistente.

## Come fare un controllo se una directory esiste in Ruby

Per verificare se una directory esiste in Ruby, possiamo utilizzare il metodo `Dir.exist?` seguito dal percorso della directory che vogliamo controllare. Vediamo un esempio di codice:

```Ruby
if Dir.exist?("/path/to/directory")
    puts "La directory esiste!"
else
    puts "La directory non esiste."
end
```

In questo esempio, stiamo utilizzando un semplice condizionale per verificare se la directory specificata esiste o meno. Se il risultato del metodo `Dir.exist?` è `true`, stamperemo il messaggio "La directory esiste!", altrimenti stamperemo "La directory non esiste.".

## Approfondimento sul controllo di esistenza di una directory

Il metodo `Dir.exist?` restituisce un valore booleano, quindi può essere utilizzato anche all'interno di espressioni booleane. Possiamo inoltre utilizzare il metodo `File.directory?` per effettuare lo stesso controllo sulla presenza di una directory, ma in questo caso dobbiamo passare la stringa del percorso completo al metodo.

E' importante notare che il metodo `Dir.exist?` restituirà `false` anche se la directory specificata è vuota o non contiene nessun file, quindi è importante verificare anche la presenza di file al suo interno se necessario.

## Vedi anche
- [Ruby Docs - Dir Class](https://ruby-doc.org/core/Dir.html)
- [Ruby Docs - File Class](https://ruby-doc.org/core/File.html)
- [Come leggere, scrivere e creare file in Ruby](https://www.linode.com/docs/development/ruby/how-to-read-write-and-create-files-in-ruby/)