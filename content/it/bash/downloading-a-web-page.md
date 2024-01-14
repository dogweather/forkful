---
title:                "Bash: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web può essere utile per diversi motivi. Può aiutare a creare uno snapshot della pagina in quel momento, ad esempio per scopi di archiviazione o per analisi future. Inoltre, può essere utile per automizzare alcune operazioni, come ad esempio scaricare regolarmente un rapporto o un file di dati.

## Come Fare

Per scaricare una pagina web utilizzando Bash, è necessario utilizzare lo strumento di riga di comando `curl`. Assicurati di averlo installato sul tuo sistema e poi scrivi il seguente comando:

```
curl -O <url>
```

Sostituisci `<url>` con l'indirizzo web della pagina che desideri scaricare. Questo comando salverà la pagina web sul tuo computer con lo stesso nome della pagina originale. Se vuoi specificare un nome diverso, puoi utilizzare la seguente opzione:

```
curl -o <nome-file> <url>
```

Con questo comando, specificarei il nome del file che vuoi utilizzare invece di utilizzare quello della pagina web. Inoltre, curl consente anche di specificare un percorso in cui salvare il file, utilizzando l'opzione `-O` o `-o` insieme al percorso desiderato.

È inoltre possibile scaricare più pagine contemporaneamente utilizzando l'opzione `-O` o `-o` più di una volta in un unico comando, separando gli URL con uno spazio.

## Approfondimento

Ci sono alcune opzioni aggiuntive che possono essere utili quando si scaricano pagine web con curl. Una di queste è l'opzione `-L`, che segue eventuali reindirizzamenti della pagina. Inoltre, è possibile impostare un limite di tempo per la richiesta utilizzando l'opzione `-m` seguita dal numero di secondi.

Per fare ancora di più con gli script Bash, si può anche estrarre informazioni specifiche dalla pagina web scaricata utilizzando strumenti di parsing come `grep`, `awk` o `sed`.

## Vedi Anche

- [Documentazione di curl](https://curl.haxx.se/docs/manpage.html)
- [Guida di Bash](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Tutorial su grep, awk e sed](https://www.gnu.org/software/sed/manual/sed.html)