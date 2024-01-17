---
title:                "Scaricare una pagina web"
html_title:           "Elm: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Ciao lettori italiani! Oggi parleremo di Elm, un linguaggio di programmazione moderno e altamente funzionale. In questa guida, impareremo come scaricare una pagina web utilizzando Elm e perché i programmatori lo fanno. Senza perdere altro tempo, iniziamo!

## Cos'è e perché?

Scaricare una pagina web significa ottenere il contenuto di una pagina web da un server remoto e visualizzarlo sul nostro dispositivo. Questo è importante per i programmatori perché consente loro di creare applicazioni web interattive, come ad esempio un sito di e-commerce o un social network.

## Come fare:

Per scaricare una pagina web in Elm, possiamo utilizzare la funzione integrata `Http.get`. Vediamo un esempio:

```
Elm.Http.get "https://www.example.com/" 
    .send <| Http.expectString loaded
```

In questo esempio, stiamo scaricando la pagina web di "www.example.com". Successivamente, utilizziamo `send` per inviare la richiesta e `expectString` per indicare che ci aspettiamo una stringa come risultato. Infine, passiamo la funzione `loaded` come parametro, che verrà eseguita quando la pagina sarà stata scaricata.

Se vogliamo mostrare il contenuto della pagina web dopo che è stata scaricata, possiamo utilizzare la funzione `Debug.log` per stampare il risultato:

```
loaded result =
    Debug.log "Contenuto della pagina:" result
```

L'output dovrebbe essere una stringa con il contenuto della pagina web. Fantastico, abbiamo scaricato con successo una pagina web utilizzando Elm!

## Approfondimenti:

Se sei interessato a saperne di più su come Elm gestisce le richieste HTTP, puoi leggere la documentazione ufficiale [qui] (https://package.elm-lang.org/packages/elm/http/latest/Http#request). Inoltre, ci sono anche alternative per scaricare una pagina web in Elm, come ad esempio il modulo `Browser` che viene utilizzato per creare applicazioni web in Elm.

## Vedi anche:

- [Documentazione ufficiale Elm] (https://elm-lang.org/docs)
- [Decifrare le basi di Elm] (https://www.futurelearn.com/courses/learn-elm-basics) (corso online gratuito in inglese)

E questo conclude la nostra guida su come scaricare una pagina web in Elm. Speriamo che ti sia stato utile e ti sia dato un'idea di come Elm sia un linguaggio di programmazione potente e facile da imparare. Continua a praticare e non vediamo l'ora di vedere cosa riuscirai a creare con Elm! Ciao!