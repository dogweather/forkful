---
title:                "Scaricare una pagina web"
html_title:           "Ruby: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Perché

Scaricare una pagina web può essere utile per numerosi motivi, ad esempio per elaborare i dati contenuti nella pagina, analizzarne il contenuto o visualizzarla offline.

# Come

Per scaricare una pagina web in Ruby, possiamo utilizzare la gemma "net/http". Ecco un esempio di codice che scarica una pagina web e ne stampa il contenuto:

```Ruby
require 'net/http'

response = Net::HTTP.get(URI('https://www.example.com/'))
puts response
```

Questo codice utilizza il metodo `get` della classe `Net::HTTP` per effettuare una richiesta GET all'URL specificato. Successivamente, il contenuto della risposta viene stampato a schermo.

# Approfondimento

La gemma "net/http" utilizza il protocollo HTTP per comunicare con i server web e scaricare il contenuto di una pagina. Per personalizzare la richiesta, possiamo utilizzare il metodo `request` invece di `get` e specificare il tipo di metodo HTTP (ad esempio GET, POST, PUT) e i parametri della richiesta.

Ecco un esempio di codice che effettua una richiesta POST e passa dei parametri alla pagina web:

```Ruby
require 'net/http'

url = URI('https://www.example.com/login')
params = { username: 'user', password: 'password' }

response = Net::HTTP.post_form(url, params)
puts response
```

In questo caso, la pagina web viene eseguita come una richiesta POST e nel parametro `params` vengono passati il nome utente e la password come dati.

# Vedi anche

- Documentazione ufficiale di Ruby sulla gemma "net/http": https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/index.html
- Approfondimenti sul protocollo HTTP: https://developer.mozilla.org/it/docs/Web/HTTP