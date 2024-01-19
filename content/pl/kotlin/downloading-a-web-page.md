---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie strony internetowej oznacza odczytywanie jej zawartości do Twojego programu. Programiści robią to, aby przetwarzać dane z internetu - od skrapowania danych do automatycznego przeglądania stron.

## Jak to zrobić:

Możemy pobrać stronę internetową przy użyciu biblioteki `Ktor`. Poniższy kod demonstruje, jak to zrobić:

```Kotlin
import io.ktor.client.*
import io.ktor.client.request.*

suspend fun main() {
    HttpClient().use { klient ->
        val strona = klient.get<String>("http://wp.pl")
        println(strona)
    }
}
```

Jako wynik, pokaże zawartość strony wp.pl.

## Wgłębny nurkowanie:

1. Kontekst historyczny: Początkowo, programiści musieli samodzielnie zaimplementować funkcje pobierania stron, co było trudne i niewygodne.
2. Alternatywy: Oprócz `Ktor`, można użyć innych bibliotek w Kotlinie, takich jak `Jsoup` albo `Fuel`.
3. Szczegóły implementacji: `Ktor` działa asynchronicznie, nie blokując głównego wątku podczas pobierania strony.

## Zobacz także:

1. Dokumentacja `Ktor`: [link](https://ktor.io/)
2. Przewodnik `Jsoup`: [link](https://jsoup.org/cookbook/)
3. Przewodnik `Fuel`: [link](https://fuel.gitbook.io/documentation/)