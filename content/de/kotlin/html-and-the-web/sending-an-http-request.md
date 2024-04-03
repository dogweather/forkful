---
date: 2024-01-20 18:00:08.705201-07:00
description: "How to: (Wie geht das:) Kotlin bietet mehrere M\xF6glichkeiten, HTTP-Anfragen\
  \ zu senden. Hier verwenden wir die Bibliothek `khttp`, die einfache und direkte\u2026"
lastmod: '2024-03-13T22:44:53.843684-06:00'
model: gpt-4-1106-preview
summary: "Kotlin bietet mehrere M\xF6glichkeiten, HTTP-Anfragen zu senden."
title: Einen HTTP-Request senden
weight: 44
---

## How to: (Wie geht das:)
Kotlin bietet mehrere Möglichkeiten, HTTP-Anfragen zu senden. Hier verwenden wir die Bibliothek `khttp`, die einfache und direkte Aufrufe ermöglicht.

```kotlin
import khttp.get

fun main() {
    val response = get("https://jsonplaceholder.typicode.com/posts/1")

    println("Status Code: ${response.statusCode}")
    println("Body: ${response.text}")
}
```

Output:

```
Status Code: 200
Body: {
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
}
```

## Deep Dive (Tiefergehendes)
Historisch gesehen, Java-Programmierer nutzten Bibliotheken wie Apache HttpClient oder die `java.net`-Pakete. Kotlin baut darauf auf, bietet aber auch eigene Bibliotheken. Alternativen zu `khttp` sind unter anderem OkHttp und Retrofit – beide beliebt und leistungsfähig. OkHttp operiert eher niedrigstufig und flexibel, Retrofit hingegen bietet ein deklaratives API-Design, das die Arbeit mit Endpunkten stark vereinfacht.

Die korrekte Handhabung von HTTP-Anfragen in Kotlin erfordert Verständnis für Konzepte wie asynchrone Programmierung, da Netzwerkoperationen Zeit beanspruchen und man den Main-Thread nicht blockieren möchte. Modernes Kotlin nutzt Coroutines für diesen Zweck, die leichtgewichtig und effizient sind.

## See Also (Siehe auch)
- [khttp documentation](https://khttp.readthedocs.io)
- [OkHttp GitHub repository](https://github.com/square/okhttp)
- [Retrofit GitHub repository](https://github.com/square/retrofit)
- [Kotlin Coroutines on kotlinx.coroutines GitHub](https://github.com/Kotlin/kotlinx.coroutines)
