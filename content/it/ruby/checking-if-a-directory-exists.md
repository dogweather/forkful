---
title:    "Ruby: Verifica dell'esistenza di una directory"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Controllare se una directory esiste è un'operazione fondamentale nella programmazione Ruby. È necessario verificare l'esistenza di una directory per garantire che il codice funzioni correttamente e per gestire eventuali errori che possono verificarsi.

## Come Fare

Per controllare se una directory esiste in Ruby, è possibile utilizzare il metodo `.exist?` del modulo `File`. Questo metodo restituisce un valore booleano (vero o falso) in base all'esistenza della directory specificata.

```
Ruby #

require 'file'

puts File.exist? ("/path/to/directory")  # restituisce true se esiste
puts File.exist? ("/path/to/nonexistent/directory") # restituisce false se non esiste
```

Se si vuole controllare l'esistenza di una directory relativa al percorso del file in cui viene eseguito il codice, è possibile utilizzare il metodo `.expand_path` per ottenere il percorso assoluto della directory.

```
Ruby #

require 'file'

puts File.exist? (File.expand_path("../directory", __FILE__))  # controlla un percorso relativo rispetto al file corrente
```

## Approfondimento

Quando si utilizza il metodo `File.exist?` per verificare l'esistenza di una directory, è importante tenere presente che il metodo non controlla solo la presenza di una directory, bensì anche di qualsiasi file o link che abbia lo stesso nome. Inoltre, è possibile che il permesso di scrittura sulla directory non sia correttamente configurato, quindi è importante gestire gli errori di permesso in modo appropriato nel proprio codice.

## Vedi Anche

- [Documentazione ufficiale di Ruby su File.exist?](https://ruby-doc.org/core-2.7.0/File.html#method-c-exist-3F)
- [Come controllare l'esistenza di una directory in Ruby](https://www.rubyguides.com/2015/10/check-if-a-file-directory-exists/)
- [Gestione degli errori di permesso in Ruby](https://www.honeybadger.io/blog/handling-permission-denied-errors-in-ruby/)