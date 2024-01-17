---
title:                "Ściąganie strony internetowej"
html_title:           "Kotlin: Ściąganie strony internetowej"
simple_title:         "Ściąganie strony internetowej"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?
Pobieranie stron internetowych polega na ściąganiu zawartości witryn z internetu i wyświetlaniu ich na naszych urządzeniach. Programiści wykorzystują to narzędzie do pobierania danych z internetu i wykorzystywania ich w swoim kodzie.

## Jak to zrobić:
```Kotlin
fun main(args: Array<String>) {
  val url = URL("https://www.example.com")
  val connection = url.openConnection()
  val input = BufferedReader(InputStreamReader(connection.getInputStream()))
  input.use { input ->
    input.lines().forEach { line -> println("$line\n") }
  }
}
```
Ten przykład pokazuje, jak otworzyć połączenie z podanym adresem URL i pobierać jego zawartość. W tym przypadku, używamy ```BufferedReader``` aby wyświetlić zawartość linijka po linijce.

## Głębsza przeprawa:
Pobieranie stron internetowych jest jednym z podstawowych zastosowań programowania. Już w latach 90. użycie protokołu HTTP stało się powszechne w celu pobierania stron internetowych. Alternatywą dla niskopoziomowego używania ```URLConnection``` jest użycie bibliotek takich jak Retrofit, które znacznie ułatwiają pobieranie i przetwarzanie danych.

## Zobacz również:
- [Kotlin's URL Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-u-r-l/)
- [Retrofit Library](https://square.github.io/retrofit/)