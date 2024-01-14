---
title:    "Ruby: Scrivere sull'errore standard"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere sullo standard error può essere utile durante la ricerca di errori nel proprio codice. Questa pratica permette di visualizzare gli errori in una finestra separata, consentendo una migliore traccia e risoluzione dei problemi.

## Come Fare

Per scrivere sullo standard error in Ruby, è possibile utilizzare il metodo "STDERR.puts". In questo modo, possiamo stampare un messaggio di errore sullo standard error invece che sullo standard output. Ecco un esempio di codice:

```Ruby
begin
  # codice che può causare un errore
  1/0
rescue ZeroDivisionError => e
  STDERR.puts "Errore: #{e.message}"
end
```
Questo codice crea un errore di divisione per zero e stampa un messaggio di errore sullo standard error utilizzando "STDERR.puts".

L'output sarà il seguente:

``` 
Errore: divided by 0
```

Invece, se utilizziamo il metodo "puts", il messaggio di errore sarebbe stampato sullo standard output.

## Approfondimento

Scrivere sullo standard error può essere particolarmente utile quando si utilizzano applicazioni web. In questi casi, gli errori vengono registrati sullo standard error e possono essere consultati in un file di log. Inoltre, è possibile impostare il livello di gravità degli errori, in modo da poter visualizzare solo quelli più critici.

Inoltre, è possibile personalizzare il messaggio di errore e aggiungere informazioni utili, come il timestamp e il percorso del file in cui si è verificato l'errore.

## Vedere Anche

- [Documentazione su STDERR e STDOUT in Ruby](https://ruby-doc.org/core-2.5.1/IO.html#concept-stdios)
- [Utilizzo dei file di log per il debugging in Ruby on Rails](https://guides.rubyonrails.org/debugging_rails_applications.html#the-rails-logger)
- [Errore di divisione in Ruby](https://ruby-doc.org/core-2.5.1/ZeroDivisionError.html)