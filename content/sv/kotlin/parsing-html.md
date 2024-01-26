---
title:                "Tolka HTML"
date:                  2024-01-20T15:32:32.502103-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
HTML-parsing är när vi extraherar data från HTML-kod, lite som att plocka äpplen från ett träd. Vi gör det för att använda webbinnehållet i våra appar, som datakälla eller för att analysera webbsidor.

## Så här gör du:
I Kotlin använder vi ofta biblioteket Jsoup för att göra HTML-parsing. Det är smidigt och kraftfullt. Här är ett snabbt exempel:

```kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Hej Sverige!</title></head><body><p>Välkommen till Kotlin.</p></body></html>"
    val doc = Jsoup.parse(html)
    
    val title = doc.title()
    println(title)  // Skriver ut: Hej Sverige!

    val pText = doc.select("p").first()?.text()
    println(pText)  // Skriver ut: Välkommen till Kotlin.
}
```

## Djupdykning
HTML-parsing är inte nytt. I Java-eran använde vi SAX och DOM, men de krävde mycket kod och var klumpiga. Jsoup förändrade spelet genom sin "jQuery-liknande" syntax som förenklar parsning och manipulation av HTML.

Alternativ till Jsoup skulle kunna vara HtmlUnit eller Kotlinx.html, men Jsoup är ofta favoriten på grund av dess enkelhet och tillförlitlighet.

När du implementerar parsing, kom ihåg att respektera webbsidors användningsvillkor och robots.txt-filer för att undvika juridiska problem.

## Se även
- Jsoup's officiella webbplats: [https://jsoup.org/](https://jsoup.org/)
- Kotlinx.html GitHub-repo: [https://github.com/Kotlin/kotlinx.html](https://github.com/Kotlin/kotlinx.html)
- HtmlUnit's webbplats: [http://htmlunit.sourceforge.net/](http://htmlunit.sourceforge.net/)
