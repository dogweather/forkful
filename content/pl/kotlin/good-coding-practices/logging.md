---
title:                "Rejestrowanie zdarzeń"
aliases:
- /pl/kotlin/logging.md
date:                  2024-01-26T01:08:23.836062-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Logowanie, w swojej istocie, jest praktyką rejestrowania zdarzeń i danych z aplikacji oprogramowania do zewnętrznego wyjścia, takiego jak plik lub konsola. Programiści logują informacje, aby śledzić przepływ kodu, diagnozować problemy i obserwować zachowanie aplikacji w rzeczywistym świecie, dostarczając krytycznych wglądów, których nie można uzyskać tak efektywnie w żaden inny sposób.

## Jak to zrobić:

W Kotlinie logowanie może być wykonywane za pomocą wbudowanej funkcji `println()` w prostych przypadkach lub za pomocą bardziej zaawansowanych bibliotek takich jak SLF4J z Logbackiem czy Log4j dla zaawansowanych potrzeb.

Poniżej znajduje się podstawowy przykład użycia `println()`:

```Kotlin
fun main() {
    println("Prosty komunikat logowania: Aplikacja uruchomiona.")
    // ... tutaj logika aplikacji ...
    try {
        // Symulacja błędu
        throw Exception("Symulowany błąd")
    } catch (e: Exception) {
        println("Komunikat błędu w logu: " + e.message)
    }
}
```

Wyjście:
```
Prosty komunikat logowania: Aplikacja uruchomiona.
Komunikat błędu w logu: Symulowany błąd
```

A oto fragment kodu używający SLF4J z Logbackiem skonfigurowanym:

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("Strukturyzowany komunikat logowania: Aplikacja uruchomiona.")
    // ... tutaj logika aplikacji ...
    try {
        // Symulacja błędu
        throw Exception("Symulowany błąd")
    } catch (e: Exception) {
        logger.error("Strukturyzowany komunikat błędu w logu: ", e)
    }
}
```

Zakładając odpowiednią konfigurację Logbacka, wyjście mogłoby być sformatowane i mogłoby wyglądać mniej więcej tak, gdy jest zapisywane do pliku logu:
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - Strukturyzowany komunikat logowania: Aplikacja uruchomiona.
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - Strukturyzowany komunikat błędu w logu: 
java.lang.Exception: Symulowany błąd
   at com.myapp.Main.main(Main.kt:10)
```

## Szczegółowa analiza

Historycznie rzecz biorąc, logowanie w oprogramowaniu rozwijało się wraz ze wzrastającą złożonością aplikacji i systemów. Proste instrukcje drukowania były wystarczające na wczesnym etapie, gdzie programy często były uruchamiane i debugowane przez samego developera. Ale w miarę jak systemy były sieciowane i działały w różnych środowiskach oraz dla różnych użytkowników, niezawodny i trwały system logowania stał się kluczowy.

Zanim Kotlin stał się popularny, programiści Java szeroko adoptowali biblioteki takie jak Log4j, a później SLF4J. Zainspirowały one podobne praktyki w Kotlinie, wykorzystując interoperacyjność Kotlina z bibliotekami Javy. SLF4J działa jako warstwa abstrakcji, pozwalająca na wymianę rzeczywistej implementacji logowania – zwykle wybór pada na Logback lub Log4j2.

Kotlin pozwala również na wieloplatformowe rozwiązania logowania, które działają zarówno w JVM, JavaScript, jak i Native, na przykład za pomocą mechanizmu `expect`/`actual`, który abstrahuje implementacje specyficzne dla platformy.

W kontrast do dedykowanych bibliotek logowania, println pozostaje najprostszą formą logowania, ponieważ nie wymaga dodatkowej konfiguracji ani zależności; jednak zazwyczaj nie jest odpowiedni dla aplikacji produkcyjnych z powodu braku funkcji takich jak poziomy logowania, rotacja logów i strukturyzowane formaty.

Inne powszechne funkcje zaawansowanych ram logowania obejmują:

- Poziomy logowania (DEBUG, INFO, WARN, ERROR, itd.) do kategoryzowania pilności komunikatów logów.
- Wyjście do różnych miejsc docelowych, jak konsola, plik, bazy danych lub usługi sieciowe.
- Automatyczna rotacja logów i polityki zatrzymywania danych.
- Obsługa rozproszonego śledzenia dla architektury mikroserwisów.
- Strukturyzowane logowanie przy użyciu formatów takich jak JSON, które dobrze integrują się z systemami analizy logów.

Te narzędzia i funkcje są kluczowe dla utrzymania niezawodnego i obserwowalnego systemu, szczególnie w skomplikowanych, rozproszonych lub wysoko skalowalnych środowiskach.

## Zobacz również

Aby dowiedzieć się więcej i uzyskać wgląd w logowanie w Kotlinie, sprawdź:

- SLF4J (Prosta Fasada Logowania dla Javy) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback, następca Log4j [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- Dokumentacja Kotlin Multiplatform o deklaracjach 'expect' i 'actual': [https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- Przewodnik po strukturyzowanym logowaniu w Kotlinie: [https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)
