---
title:                "Scaricare una pagina web"
html_title:           "Haskell: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Perché

Se sei interessato ad estrarre informazioni da una pagina web o ad utilizzare i suoi contenuti in un tuo progetto, allora scaricare una pagina web utilizzando Haskell può essere la soluzione che stai cercando. Inoltre, imparare a utilizzare Haskell per il web può arricchire le tue competenze di programmazione e aprirti ad un nuovo mondo di possibilità.

##Come Fare

Per prima cosa, assicurati di aver installato il compilatore di Haskell sul tuo computer. Dopo aver fatto ciò, segui questi semplici passaggi per scaricare una pagina web utilizzando Haskell.

```
Haskell
import Network.HTTP.Simple -- Importa il modulo per la gestione di richieste HTTP

-- Esegue una richiesta GET all'URL specificato
pagina <- httpGet "https://www.example.com"

-- Legge il contenuto della pagina e lo converte in una stringa
contenuto <- getResponseBody pagina

-- Stampa il contenuto della pagina
print contenuto
```

Questo codice esegue una richiesta HTTP all'URL specificato e legge il contenuto della pagina in formato di stringa. Utilizzando il modulo `Network.HTTP.Simple`, non è necessario preoccuparsi di gestire le connessioni HTTP, poiché il modulo si occupa di questo per te. Inoltre, puoi utilizzare anche altri metodi di richiesta, come POST o PUT, a seconda delle tue esigenze.

##Deep Dive

Oltre alla semplice richiesta di una pagina web, Haskell consente di effettuare operazioni più avanzate, come il parsing del contenuto della pagina per estrarre solo le informazioni di interesse. Ci sono molti pacchetti disponibili per il parsing di HTML, come `tagsoup` o `html-conduit`, che possono semplificare notevolmente questa operazione. Inoltre, puoi utilizzare librerie di scraping web come `scrape` o `scraper` per facilitare ulteriormente il processo di estrazione di dati da una pagina web.

##Vedi Anche

- [Haskell for Beginners](https://www.haskell.org/learn/)
- [Haskell Web Programming](https://haskellwebprogramming.com/)
- [Scraper Package](https://hackage.haskell.org/package/scraper)