---
title:                "Scrivere un file di testo"
html_title:           "Ruby: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo può sembrare banale, ma in realtà è una delle abilità fondamentali per qualsiasi programmatore di Ruby. Potresti aver bisogno di creare un file di configurazione, salvare dati in un formato leggibile, o addirittura scrivere un file di log per il tuo programma.

## Come fare

Per scrivere un file di testo in Ruby, possiamo utilizzare il metodo `File.write`. Ad esempio, se vogliamo creare un file chiamato "esempio.txt" e scrivere al suo interno la stringa "Ciao mondo!", possiamo fare così:

```
File.write("esempio.txt", "Ciao mondo!")
```

Questo creerà un nuovo file chiamato "esempio.txt" nella stessa cartella in cui si trova il tuo programma, e scriverà al suo interno la frase "Ciao mondo!".

## Approfondimento

Oltre al metodo `File.write`, esistono anche altri modi per scrivere un file in Ruby. Ad esempio, possiamo utilizzare il metodo `File.open` per aprire un file in modalità scrittura e poi utilizzare il metodo `write` per scrivere al suo interno. Inoltre, possiamo anche specificare il formato dei dati da scrivere, come nel seguente esempio:

```
File.open("numeri.txt", "w") do |file|
  file.write([1, 2, 3].join("\n"))
end
```

Questo creerà un file chiamato "numeri.txt" e scriverà all'interno una lista di numeri separati da una nuova riga.

## Vedi anche

- La documentazione ufficiale di Ruby sul metodo `File.write`: https://ruby-doc.org/core-2.7.1/File.html#method-c-write
- Un tutorial su come scrivere un file di testo in Ruby: https://www.rubyguides.com/2015/05/working-with-files-ruby/
- Un esempio di utilizzo del metodo `File.open` per scrivere un file in modalità scrittura: https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-ruby