---
title:                "Scaricare una pagina web."
html_title:           "Kotlin: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché 

Scaricare una pagina web può essere utile per molti motivi: salvare una pagina di riferimento per leggerla offline, recuperare informazioni da un sito per elaborarle o semplicemente per avere una copia locale di una pagina importante.

## Come Fare

Per scaricare una pagina web in Kotlin, è possibile utilizzare la libreria `OkHttp` che offre un'interfaccia semplice ed efficiente per le operazioni di rete.

Per iniziare, è necessario aggiungere la dipendenza di `OkHttp` nel `build.gradle`:

```
dependencies {
    implementation("com.squareup.okhttp3:okhttp:4.9.0")
}
```

Una volta aggiunta la dipendenza, è possibile creare una classe `Downloader` che gestirà il download della pagina web. Inizializziamo un oggetto di tipo `OkHttpClient` che ci permetterà di eseguire la richiesta HTTP:

```Kotlin
class Downloader {
  private val client = OkHttpClient()
}
```

Nella nostra classe, aggiungiamo un metodo `download` che accetterà come parametro l'URL della pagina web che vogliamo scaricare:

```Kotlin
fun download(url: String) {
  val request = Request.Builder()
      .url(url)
      .build()
  
  client.newCall(request).execute().use { response ->
    if(!response.isSuccessful) throw IOException("Unexpected code $response")

    println(response.body()?.string())
  }
}
```

In questo esempio, l'URL viene passato come parametro al metodo `download` e viene utilizzato per creare una richiesta di tipo `GET` utilizzando la classe `Request` di `OkHttp`. Successivamente, utilizziamo il metodo `execute()` per eseguire la richiesta e aspettiamo una risposta. Infine, stampiamo il body della risposta utilizzando il metodo `string()`.

Ora possiamo utilizzare il nostro `Downloader` nella nostra classe principale per scaricare una pagina web. Ad esempio:

```Kotlin
fun main() {
  val downloader = Downloader()
  downloader.download("https://www.google.com")
}
```

Questo esempio scaricherà la pagina di Google e stamperà il suo contenuto sulla console.

## Deep Dive

Oltre al semplice download di una pagina web, `OkHttp` offre anche molte altre funzionalità come il supporto ai protocolli HTTP/2 e HTTP/1, la gestione delle connessioni, la gestione delle cache e molto altro. Inoltre, è possibile impostare header personalizzati e gestire le richieste e le risposte in modo asincrono.

Per ulteriori informazioni su `OkHttp`, è possibile consultare la documentazione ufficiale su [GitHub](https://github.com/square/okhttp) o su [Square's website](https://square.github.io/okhttp/).

## Vedi Anche

- [Kotlin Official Website](https://kotlinlang.org/)
- [Kotlin Tutorial for Beginners](https://www.tutorialspoint.com/kotlin/index.htm)