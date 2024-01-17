---
title:                "Scaricare una pagina web."
html_title:           "Ruby: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Scaricare una pagina web significa ottenere il suo contenuto e salvarlo sul proprio computer. I programmatori spesso lo fanno perché hanno bisogno di analizzare il codice HTML di una pagina o di utilizzare i suoi dati per scopi specifici.

## Come fare:

```Ruby
require 'open-uri'

page = open("https://www.google.com") # sostituisci l'URL con quello desiderato
puts page.read # stampa il contenuto della pagina
```

```
<!DOCTYPE html>
<html>
  <head>
    <title>Google</title>
    ...
  </head>
  ...
</html>
```

## Approfondimenti:

Scaricare una pagina web è diventato sempre più importante con lo sviluppo del web. In passato, i programmatori utilizzavano principalmente linguaggi come Perl e Python per effettuare questa operazione. Ora, grazie a Ruby e alle sue librerie, è possibile farlo più facilmente e in meno righe di codice.

## Vedi anche:

- Il file di libreria OpenURI nella documentazione di Ruby: https://ruby-doc.org/stdlib-2.7.1/libdoc/open-uri/rdoc/OpenURI.html
- Un tutorial su come scaricare pagine web con Ruby: https://www.tutorialspoint.com/ruby/ruby_web_page_download.htm