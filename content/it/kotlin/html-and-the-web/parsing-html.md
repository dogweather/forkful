---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:18.980108-07:00
description: 'Come fare: Kotlin semplifica il parsing dell''HTML con librerie come
  Jsoup. Ecco come si fa.'
lastmod: '2024-03-13T22:44:43.389193-06:00'
model: gpt-4-0125-preview
summary: Kotlin semplifica il parsing dell'HTML con librerie come Jsoup.
title: Analisi del HTML
weight: 43
---

## Come fare:
Kotlin semplifica il parsing dell'HTML con librerie come Jsoup. Ecco come si fa:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Pagina di Prova</title></head><body><p>Questo è un test.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("Titolo: $title")  // Output: Titolo: Pagina di Prova

    val pText = doc.select("p").first()?.text()
    println("Paragrafo: $pText")  // Output: Paragrafo: Questo è un test.
}
```

Catturiamo il titolo e il testo del paragrafo, toccando appena la superficie di ciò che Jsoup può fare. Ma è un inizio.

## Approfondimento:
Prima di Kotlin, Java era il linguaggio di riferimento per questo, spesso in modo goffo. Jsoup ha cambiato le carte in tavola fornendo un approccio simile a jQuery. Tuttavia, il parsing dell'HTML non è esclusivo di Jsoup; esistono altre librerie come HtmlUnit o persino le regex (anche se sconsigliate). Con Jsoup, ti assicuri che il tuo parsing rispetti la struttura del documento. Utilizza un modello DOM, abilitando la selezione e manipolazione degli elementi. È resiliente, inoltre—può parsare anche l'HTML più disordinato.

## Vedi Anche:
Approfondisci con Jsoup:

- Documentazione ufficiale di Jsoup: https://jsoup.org/
- Libro "Kotlin per sviluppatori Android": https://antonioleiva.com/kotlin-android-developers-book/
- Sito ufficiale del linguaggio di programmazione Kotlin: https://kotlinlang.org/

Per discussioni più ampie e tutorial sul web scraping e parsing:

- Web Scraping con Kotlin e Jsoup: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- Parsing dell'HTML su Android con Kotlin e Jsoup: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
