---
title:                "Kotlin: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach dostęp do informacji jest niezwykle ważny. Często potrzebujemy szybkiego dostępu do stron internetowych, aby znaleźć potrzebne nam informacje. Dlatego warto poznać, jak pobrać stronę internetową za pomocą języka Kotlin.

## Jak to zrobić

Aby pobrać stronę internetową za pomocą Kotlina, możemy użyć biblioteki Jsoup. Poniżej przedstawiam przykładowy kod, który pobiera zawartość strony "https://www.example.com" i wyświetla ją w konsoli.

```Kotlin
val doc = Jsoup.connect("https://www.example.com").get()
println("Zawartość strony: " + doc.body())
```

Możemy również dokonać dalszej obróbki pobranej zawartości, np. wyciągnąć konkretne elementy strony za pomocą selektorów CSS.

```Kotlin
val titles = doc.select("h1")
println("Tytuł strony: " + titles[0].text())
```

Powyżej przedstawione przykłady pokazują, jak w prosty i skuteczny sposób pobrać stronę internetową za pomocą Kotlina.

## Głębszy zanurkować

Kotlin jest językiem programowania ogólnego przeznaczenia, ale dzięki swoim funkcjom funkcyjnym i obsłudze kodu asynchronicznego, jest także idealnym narzędziem do pobierania stron internetowych. Jsoup, którego użyliśmy w przykładzie, oferuje wiele dodatkowych funkcji, które warto poznać i wykorzystać w swoich projektach.

## Zobacz również

- [Dokumentacja Jsoup](https://jsoup.org/)
- [Kurs programowania w języku Kotlin na Udemy](https://www.udemy.com/course/kotlin-dla-poczatkujacych/)
- [Oficjalna strona języka Kotlin](https://kotlinlang.org/)