---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:37.346264-07:00
description: "Parsowanie HTML oznacza rozbieranie znacznik\xF3w strony internetowej\
  \ na co\u015B, co program mo\u017Ce zrozumie\u0107 i manipulowa\u0107. Programi\u015B\
  ci parsuj\u0105 HTML w celu\u2026"
lastmod: '2024-03-13T22:44:35.362793-06:00'
model: gpt-4-0125-preview
summary: "Parsowanie HTML oznacza rozbieranie znacznik\xF3w strony internetowej na\
  \ co\u015B, co program mo\u017Ce zrozumie\u0107 i manipulowa\u0107. Programi\u015B\
  ci parsuj\u0105 HTML w celu\u2026"
title: "Analiza sk\u0142adniowa HTML"
weight: 43
---

## Co i dlaczego?
Parsowanie HTML oznacza rozbieranie znaczników strony internetowej na coś, co program może zrozumieć i manipulować. Programiści parsują HTML w celu ekstrakcji danych, automatyzacji interakcji webowych lub migracji treści między systemami.

## Jak to zrobić:
Kotlin ułatwia parsowanie HTML dzięki bibliotekom takim jak Jsoup. Oto jak to zrobić:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Przykładowa strona</title></head><body><p>To jest test.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("Tytuł: $title")  // Wyjście: Tytuł: Przykładowa strona

    val pText = doc.select("p").first()?.text()
    println("Paragraf: $pText")  // Wyjście: Paragraf: To jest test.
}
```

Chwytamy tytuł i tekst paragrafu, ledwo drapiąc powierzchnię tego, co Jsoup może zrobić. Ale to dobry początek.

## Głębsze zanurzenie:
Przed Kotlinem, do tego często używano Javy, nieraz niezgrabnie. Jsoup zmienił zasady gry, dostarczając podejście podobne do jQuery. Parsowanie HTML nie jest wyłączne dla Jsoup; istnieją również inne biblioteki, takie jak HtmlUnit czy nawet wyrażenia regularne (chociaż nie zaleca się ich używania). Dzięki Jsoup zapewniasz, że parsowanie respektuje strukturę dokumentu. Używa modelu DOM, umożliwiając selekcję i manipulację elementami. Jest też odporny - potrafi parsować nawet najbardziej zagmatwane HTML.

## Zobacz także:
Pogłęb swoją wiedzę na temat Jsoup:

- Oficjalna dokumentacja Jsoup: https://jsoup.org/
- Książka "Kotlin dla programistów Androida": https://antonioleiva.com/kotlin-android-developers-book/
- Oficjalna strona języka programowania Kotlin: https://kotlinlang.org/

Dla szerszych dyskusji i poradników na temat web scrapingu i parsowania:

- Web Scraping z Kotlinem i Jsoup: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- Parsowanie HTML na Androidzie z Kotlinem i Jsoup: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
