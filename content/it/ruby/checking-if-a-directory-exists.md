---
title:                "Controllare se una directory esiste"
html_title:           "Ruby: Controllare se una directory esiste"
simple_title:         "Controllare se una directory esiste"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Perché

Controllare se una directory esiste può essere utile quando si sta sviluppando un programma che ha bisogno di accedere o creare nuove directories. In questo modo si evita di incorrere in errori durante l'esecuzione del codice.

## Come fare

Per controllare se una directory esiste, si può utilizzare il metodo `Dir.exists?` che restituisce un valore booleano. Se la directory esiste, il valore sarà `true`, se non esiste il valore sarà `false`.

Ecco un esempio di codice che controlla se la directory "documents" esiste all'interno della directory corrente:

```Ruby
if Dir.exist?("documents")
  puts "La directory documents esiste!"
else
  puts "La directory documents non esiste!"
end
```

Output:

```
La directory documents esiste!
```

## Deep Dive

Il metodo `Dir.exists?` è presente nella classe `Dir` che è una sottoclasse della classe `File`. Questo significa che è possibile utilizzare anche altri metodi della classe `File` per controllare la presenza di una directory, come ad esempio `File.directory?` che restituisce lo stesso valore booleano.

Inoltre, è possibile specificare il percorso completo della directory da controllare o utilizzare una variabile che contiene il percorso. Esempio:

```Ruby
path = "/home/utente/Scrivania/"
if Dir.exists?(path)
  puts "#{path} esiste!"
else
  puts "#{path} non esiste!"
end
```

Se la directory esiste, il codice stamperà `"#{path} esiste!"`. In caso contrario, verrà stampato `"#{path} non esiste!"`.

## Vedi anche

- Documentazione ufficiale di Ruby: https://www.ruby-lang.org/it/documentation/
- Guida interattiva di Ruby: https://www.codecademy.com/learn/learn-ruby
- Esercitazioni pratiche di Ruby: https://www.rubyguides.com/