---
title:                "Ruby: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che lavora con Ruby, probabilmente hai sentito parlare del concetto di "web scraping" e dell'utilizzo della libreria `net/http` per scaricare pagine web. Ma perché dovresti farlo? Ci sono molte ragioni per cui potresti voler scaricare una pagina web, ad esempio per raccogliere dati per un progetto o per automatizzare un processo. Con Ruby, è possibile farlo in modo semplice ed efficiente.

## Come fare

Per scaricare una pagina web utilizzando Ruby, puoi utilizzare la classe `Net::HTTP`, che è inclusa nella libreria standard di Ruby. Per prima cosa, dobbiamo richiedere la pagina che desideriamo scaricare utilizzando il suo URL. Possiamo farlo utilizzando il metodo `get` e fornendo l'URL come argomento. Ad esempio, per scaricare il codice sorgente della pagina "google.it", possiamo scrivere:

```Ruby
require 'net/http'

page = Net::HTTP.get(URI('http://www.google.it/'))
```

Con questo codice, abbiamo ottenuto il codice sorgente della pagina, che è stato assegnato alla variabile `page`. Possiamo quindi utilizzare questo codice per manipolarlo e estrarre le informazioni di nostro interesse.

## Deep Dive

Ovviamente, scaricare una pagina web è solo il primo passo. Una volta ottenuto il codice sorgente, è possibile utilizzare le espressioni regolari o libreria di parsing come `Nokogiri` per estrarre informazioni specifiche dalla pagina. Inoltre, è possibile simulare il comportamento di un browser inviando richieste con i cookie appropriati o utilizzando l'autenticazione HTTP.

E se invece di semplicemente scaricare il codice sorgente, volessimo anche salvare l'intera pagina web con le risorse (immagini, stylesheet, script, ecc.)? In questo caso, possiamo utilizzare la gemma `wget` che ci permette di fare proprio questo con una semplice riga di codice:

```Ruby
require 'wget'

Wget.download('http://www.google.it/')
```

Questo è solo un assaggio di quello che è possibile fare con il download delle pagine web con Ruby. Con un po' di creatività, è possibile utilizzare questa funzionalità per automatizzare molte attività e migliorare la tua efficienza come sviluppatore.

## Vedi anche

- [Documentazione ufficiale della classe `Net::HTTP` in Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/net/http/rdoc/Net/HTTP.html)
- [Gemma `Nokogiri` per il parsing di pagine web in Ruby](https://www.nokogiri.org/)
- [Gemma `wget` per il download di pagine web in Ruby](https://github.com/david942j/wgetrb)