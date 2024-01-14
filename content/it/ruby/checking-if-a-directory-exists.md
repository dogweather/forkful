---
title:    "Ruby: Verifica dell'esistenza di una cartella"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Perché

Una delle attività più comuni nella programmazione è la gestione dei file e delle directory. Una delle tante sfide che si incontrano è verificare l'esistenza di una directory, che può risultare utile per diverse ragioni. Vediamo insieme come fare questa operazione in Ruby.

## Come fare

È possibile verificare l'esistenza di una directory utilizzando il metodo `Dir.exist?` e passando come argomento il percorso della directory da controllare. Questo metodo restituirà `true` se la directory esiste e `false` in caso contrario.

```
# Verifica se la directory "documents" esiste
Dir.exist?("documents")
=> true
```

In alternativa, si può utilizzare il metodo `Dir.exist?` che accetta come argomento anche un oggetto `File` che rappresenta la directory. In questo modo è possibile controllare anche i permessi di lettura, scrittura ed esecuzione sulla directory.

```
directory = File.new("documents")
# Verifica se la directory esiste ed è accessibile in lettura, scrittura ed esecuzione
Dir.exists?(directory)
=> true
```

## Approfondimento

Oltre a questi metodi, ci sono alcune cose da tenere presente quando si tratta di verificare l'esistenza di una directory. Ad esempio, se il percorso specificato punta a un file anziché a una directory, il metodo restituirà comunque `false`. Inoltre, il metodo `Dir.exist?` non verifica l'esistenza della directory definitiva, ma solo di quella specifica nel percorso fornito. Se si desidera controllare l'esistenza della directory completa, è necessario utilizzare il metodo `File.exist?`.

# Vedi anche

- [Documentazione sul metodo `Dir.exist?` di Ruby](https://ruby-doc.org/core-2.7.0/Dir.html#method-c-exist-3F)
- [Documentazione sul metodo `File.exist?` di Ruby](https://ruby-doc.org/core-2.7.0/File.html#method-c-exist-3F)