---
title:                "Scaricare una pagina web"
html_title:           "Kotlin: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Cosa & Perché?

Scaricare una pagina web significa ottenere il suo contenuto e salvarlo sul nostro computer. I programmatori spesso fanno questo per analizzare il contenuto della pagina o integrarlo in altre applicazioni.

Come Fare:

```Kotlin
import java.net.URL

// Definisci l'URL della pagina web che vuoi scaricare
val url = URL("https://www.example.com")

// Usa il metodo openStream() per ottenere l'input stream del contenuto della pagina
val inputStream = url.openStream()

// Leggi il contenuto dell'input stream e salvalo in una variabile
val contenuto = inputStream.bufferedReader().use { it.readText() }

// Stampa il contenuto della pagina
println(contenuto)
```

Output:

```
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
  <meta charset="UTF-8">
  <meta http-equiv="Content-type" content="text/html; charset=UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <style type="text/css">
    body {
      background-color: #f0f0f2;
      margin: 0;
      padding: 0;
      font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
    
    ...
```

Deep Dive:

Scaricare una pagina web può risalire ai primi giorni del World Wide Web quando i programmatori stavano sviluppando metodi per accedere ai contenuti dei siti web. Oltre all'approccio mostrato sopra, ci sono altre opzioni per scaricare una pagina web utilizzando librerie esterne come Jsoup o OkHttp. Inoltre, è importante considerare l'implementazione corretta della gestione degli errori e la gestione delle eccezioni quando si scaricano pagine web.

Vedi Anche:

- [Tutorial su Jsoup in Kotlin](https://www.tutorialkart.com/kotlin/jsoup-kotlin-html-parser/ "Tutorial su Jsoup in Kotlin")
- [Documentazione ufficiale di OkHttp per Kotlin](https://square.github.io/okhttp/ "Documentazione ufficiale di OkHttp per Kotlin")