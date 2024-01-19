---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Scaricare una pagina web significa recuperare i suoi dati attraverso Internet. I programmatori fanno ciò per analizzare il contenuto della pagina, manipolare i dati o conservarli per un uso successivo.

## Come fare:

```Fish Shell
# Installa wget se non presente
if not type -q wget
 sudo apt install wget
end

# Scarica una pagina web
wget 'http://esempio.com'

# Controlla se il download è avvenuto con successo
if test -e 'index.html'
 echo 'Download riuscito!'
else
 echo 'Download fallito.'
end
```

Il codice sopra prima controlla se `wget` è installato. Se non lo è, lo installa. Poi scarica una pagina web (http://esempio.com in questo caso). Infine, verifica se il file `index.html` esiste, il che significa che il download è stato un successo.

## Approfondimento

Storicamente, il download di pagine web ha iniziato con la nascita del World Wide Web. `wget` è uno dei primi strumenti sviluppati per questo scopo.

Ci sono diverse alternative a `wget`, come `curl` e `httpie`. Ognuno ha i suoi punti di forza e debolezza, quindi la scelta dipende dalle esigenze specifiche.

Nel cuore del download di una pagina web, c'è una richiesta HTTP GET. La risposta HTTP contiene il contenuto della pagina web. `wget` salva automaticamente questo contenuto in un file.

## Vedi Anche

- [WGet man page](https://www.gnu.org/software/wget/manual/wget.html): Documento ufficiale di `wget`.
- [Curl vs Wget](https://daniel.haxx.se/docs/curl-vs-wget.html): Confronto tra `curl` e `wget`.
- [HTTPie](https://httpie.io/): Un altro strumento di download.
- [HTTP Request Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods): Informazioni sulle richieste HTTP.