---
title:                "Verifica se una directory esiste"
html_title:           "Lua: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cos'è & Perché? (What & Why?)

Verificare se una directory esiste è un'operazione che consiste nel controllare se una determinata directory è presente o meno sul filesystem. I programmatori lo fanno per evitare errori di runtime cercando di accedere a directory che potrebbero non esistere.

## Come fare: (How to)

Ruby fornisce un modo molto diretto per controllare se una directory esiste tramite il suo modulo integrato 'File'. Ecco come:

```Ruby
if File.directory?("/path/to/directory")
  puts "La directory esiste."
else
  puts "La directory non esiste."
end
```

Se la directory a '/path/to/directory' esiste, verrà stampato "La directory esiste.", altrimenti "La directory non esiste.".

## Approfondimento (Deep Dive)

Il metodo `File.directory?` è presente in Ruby da tempo. Capire come funziona può aiutarti a scrivere codice più solido e a prevenire errori potenziali.

In realtà, ci sono altre due strade che puoi percorrere per raggiungere lo stesso risultato. La prima è tramite la classe `Dir`. Puoi usare il metodo `Dir.exist?` in questo modo:

```Ruby
if Dir.exist?("/path/to/directory")
  puts "La directory esiste."
else
  puts "La directory non esiste."
end
```

La seconda via alternativa è tramite il modulo `FileTest` che fornisce il metodo `.directory?`:

```Ruby
if FileTest.directory?("/path/to/directory")
  puts "La directory esiste."
else
  puts "La directory non esiste."
end
```

Rubato dal Javascript, Ruby ha ora asincronicità, dunque sarebbe possibile creare un metodo di verifica della directory esistenza in modo asincrono.

## Per Saperne Di Più (See Also)

* [Documentazione ufficiale Ruby per File.directory?](https://ruby-doc.org/core-3.0.0/File.html#method-c-directory-3F)
* [Ruby API per Dir.exist?](https://ruby-doc.org/core-3.0.0/Dir.html#method-c-exist-3F)
* [Metodo Ruby FileTest.directory?](https://ruby-doc.org/core-3.0.0/FileTest.html#method-c-directory-3F)