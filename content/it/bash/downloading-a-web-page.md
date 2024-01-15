---
title:                "Scaricare una pagina web"
html_title:           "Bash: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Se vuoi avere un'immagine fissa di una pagina web, il download è un'ottima opzione. Può essere utile, ad esempio, per salvare una pagina che contiene informazioni importanti o per creare uno screenshot di una pagina che potresti voler condividere con qualcuno.

## Come fare

Per scaricare una pagina web utilizzando Bash, puoi utilizzare il comando `curl`, che è uno strumento di comunicazione basato su riga di comando. Ecco un esempio di come puoi scaricare una pagina web e salvarla come file:

```
curl http://www.example.com > pagina.html
```

Puoi specificare il nome del file che desideri utilizzare al posto di `pagina.html`. Puoi anche aggiungere l'opzione `-O` per utilizzare il nome della pagina web come nome del file:

```
curl -O http://www.example.com
```

Inoltre, puoi utilizzare l'opzione `-L` per seguire eventuali reindirizzamenti della pagina web e scaricare la pagina finale.

Per ulteriori informazioni su come utilizzare il comando `curl` per scaricare una pagina web, puoi consultare i documenti di Debian o la pagina di manuale di `curl`.

## Approfondimento

Il comando `curl` è molto flessibile e può essere utilizzato per eseguire diverse operazioni di trasferimento di dati. Oltre al download di una pagina web, puoi utilizzarlo per caricare file, inviare dati e accedere a risorse protette da autenticazione. Puoi anche impostare le opzioni di proxy e gestire i cookie.

Se vuoi imparare di più su come utilizzare `curl` per diverse attività di trasferimento di dati, ti consiglio di consultare la documentazione ufficiale disponibile sul sito Web di `curl`.

## Vedi anche

- [Documentazione di Debian per il comando curl](https://manpages.debian.org/testing/curl/curl.1.en.html)
- [Pagina di manuale di curl](https://curl.haxx.se/docs/manual.html)