---
title:                "Parsing html"
html_title:           "Kotlin: Parsing html"
simple_title:         "Parsing html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Se sei interessato alla creazione di applicazioni web, la conoscenza del parsing HTML è fondamentale. Attraverso il parsing, puoi estrarre e manipolare i dati all'interno di un documento HTML, rendendo possibile la creazione di applicazioni che utilizzano contenuti da fonti esterne come siti web o API.

## Come fare

Prima di tutto, dobbiamo importare la libreria Jsoup nel nostro progetto. Facciamo ciò aggiungendo la seguente dipendenza nel nostro file build.gradle:

```
implementation "org.jsoup:jsoup:1.13.1"
```

Una volta importata la libreria, possiamo iniziare ad eseguire il parsing HTML utilizzando il metodo `parse()` di Jsoup. Vediamo un esempio:

```Kotlin
val html = "<html><head><title>Il mio sito</title></head><body><h1>Ciao, mondo!</h1></body></html>"
val doc = Jsoup.parse(html)
println(doc.title()) //stamperà "Il mio sito"
```

In questo esempio, abbiamo creato una stringa contenente un frammento di codice HTML e l'abbiamo passata al metodo `parse()` di Jsoup. Il risultato è un oggetto `Document` che rappresenta la struttura del documento HTML. Utilizzando i metodi disponibili su questo oggetto, possiamo estrarre dati specifici dal documento, come il titolo nella riga successiva.

Ma il parsing HTML non riguarda solo il recupero di elementi specifici. Possiamo anche cercare elementi all'interno del documento utilizzando selettori CSS. Vediamo un altro esempio in cui vogliamo ottenere tutti gli elementi `<a>` all'interno del nostro documento:

```Kotlin
val html = "<html><body><a href="https://www.example.com">Link 1</a><a href="https://www.example2.com">Link 2</a></body></html>"
val doc = Jsoup.parse(html)
val links = doc.select("a") //seleziona tutti gli elementi <a>
for (link in links) {
    println(link.attr("href")) //stamperà "https://www.example.com" e "https://www.example2.com"
}
```

Come puoi vedere, con l'utilizzo dei selezionatori CSS, possiamo ottenere facilmente tutti gli elementi desiderati dal documento.

## Approfondimento

Il parsing HTML può essere molto utile non solo per recuperare dati, ma anche per manipolare il contenuto del documento. Con Jsoup, possiamo aggiungere o rimuovere elementi, modificare attributi e persino eseguire operazioni di sostituzione di testo.

Inoltre, possiamo anche gestire eventuali errori di parsing o condizioni di errore nel documento utilizzando metodi come `error()` e `ignoreContentType()`.

Ma ricorda che il parsing HTML può essere un'operazione pesante, quindi è importante saper valutare quando è necessario utilizzarlo e quando è possibile utilizzare altre tecniche per ottenere i dati desiderati.

## Vedi anche
- Documentazione ufficiale di Jsoup: https://jsoup.org/
- Tutorial di Kotlin su parsing HTML con Jsoup: https://kotlinexpertise.com/parse-html-in-kotlin-with-jsoup/
- Esempi di selezionatori CSS: https://www.w3schools.com/cssref/css_selectors.asp