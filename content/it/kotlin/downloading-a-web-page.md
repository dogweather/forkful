---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?

Scaricare una pagina web significa acquisire il suo codice sorgente HTML. I programmatori fanno questo per analizzare, manipolare o utilizzare i dati della pagina.

## Come fare:

Ecco come scaricare una pagina web in Kotlin, utilizzando la libreria `jsoup`.

```Kotlin
import org.jsoup.Jsoup

fun scaricaPaginaWeb(url: String): String {
    val documento = Jsoup.connect(url).get()
    return documento.html()
}

fun main() {
    val url = "http://www.esempio.com"
    println(scaricaPaginaWeb(url))
}
```

Quando esegui il codice, vedrai qualcosa come:

```
<!doctype html>
<html>
...
</html>
```

## Approfondimento

1. Contesto storico: Il web scraping è iniziato praticamente con l'Internet. All'inizio, tutto era fatto manualmente, ma da allora abbiamo sviluppato strumenti automatici come `jsoup` per Kotlin.

2. Alternative: Oltre a `jsoup`, in Kotlin si possono utilizzare anche `khttp` o `Fuel` per scaricare pagine web.

3. Dettagli di implementazione: Jsoup non scarica solo il codice HTML. Esso analizza anche il codice, costruisce un albero DOM e permette di eseguire query su di esso. Questo può essere molto utile se si desidera fare qualcosa di più complicato che semplicemente scaricare la pagina web.

## Vedere Anche

1. La documentazione ufficiale di Jsoup per ulteriori informazioni ([link](https://jsoup.org/)).
2. Una guida più approfondita al web scraping con Kotlin e Jsoup ([link](https://www.baeldung.com/kotlin-jsoup)).
3. Un tutorial video su come scaricare pagine web con Kotlin ([link](https://www.youtube.com/watch?v=ZIZjvTKVIzM)).