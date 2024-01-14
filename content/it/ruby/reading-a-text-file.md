---
title:    "Ruby: Lettura di un file di testo"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Perché

Perché qualcuno vorrebbe leggere un file di testo? La risposta è semplice: per estrarre informazioni o dati da un file e utilizzarli in un programma. In questo post, esploreremo come leggere un file di testo utilizzando il linguaggio di programmazione Ruby.

## Come

Per iniziare, abbiamo bisogno di un file di testo di esempio. Creeremo un file chiamato "esempio.txt" e lo salveremo nella stessa cartella del nostro codice Ruby. Il file di testo conterrà semplicemente alcune righe di testo.

```
Prova questa guida per imparare a leggere un file di testo con Ruby!
Questa è una riga di testo di esempio.
E questa è un'altra riga di testo.
```

Per leggere il contenuto del file di testo, dobbiamo utilizzare il metodo `File.open` e fornire il percorso del file come argomento. Utilizzeremo anche il metodo `readlines` per ottenere tutte le righe del file come un array.

```
file = File.open("esempio.txt")
righe = file.readlines
puts righe
```

Eseguendo questo codice, otterremo il seguente output:

```
Prova questa guida per imparare a leggere un file di testo con Ruby!
Questa è una riga di testo di esempio.
E questa è un'altra riga di testo.
```

Possiamo anche utilizzare un blocco di codice per leggere il file di testo. Questo è particolarmente utile quando si lavora con grandi file e si desidera gestire la lettura riga per riga.

```
File.open("esempio.txt").each do |linea|
  puts linea
end
```

L'output sarà lo stesso del codice precedente.

## Deep Dive

Ci sono altri metodi che possiamo utilizzare per leggere un file di testo con Ruby, ad esempio `File.read` per ottenere il contenuto del file come una stringa o `IO.readlines` per leggere il file linea per linea senza la necessità di un blocco di codice. Inoltre, possiamo specificare un codice di blocco opzionale con `readlines` per eseguire un'azione su ogni riga del file.

```
IO.readlines("esempio.txt") do |linea|
  puts "Lunghezza della riga: #{linea.length}"
end
```

Oltre a leggere il contenuto di un file di testo, possiamo anche scrivere o aggiungere del testo a un file utilizzando il metodo `<<` e `File.write` rispettivamente.

## Vedi anche

- [Documentazione ufficiale di Ruby su File](https://ruby-doc.org/core-3.0.0/File.html)
- [Guida di Ruby per la lettura e la scrittura di file](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Video tutorial su come leggere un file di testo con Ruby](https://www.youtube.com/watch?v=p_asc-lw53E)