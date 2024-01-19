---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem umożliwia przekazanie informacji uwierzytelniania jako część żądania HTTP. Programiści tworzą to, aby zabezpieczyć i ograniczyć dostęp do danych poprzez walidację użytkowników.

## Jak to zrobić:

Użyjemy biblioteki `Ktor` do wysyłania żądań HTTP. 

```Kotlin
import io.ktor.client.*
import io.ktor.client.features.auth.*
import io.ktor.client.features.auth.providers.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient() {
        install(Auth) {
            basic {
                username = "mojanazwa"
                password = "haslo123"
            }
        }
    }

    val response: String = client.get("http://mojserwer.pl")
    println(response)
}
```
Kiedy uruchomisz ten program, jeśli twoje uwierzytelnianie jest poprawne, otrzymasz odpowiedź od serwera z żądanymi danymi.

## Głębsze zanurzenie

Choć to jest teraz standardowe podejście, wywodzi się z początków Internetu, kiedy uwierzytelnianie było zwykle realizowane w ramach protokołu HTTP. Istnieją inne metody uwierzytelniania, takie jak uwierzytelnianie typu bearer token lub uwierzytelnianie OAuth, ale podstawowe uwierzytelnianie jest zwykle najprostszym do zaimplementowania.

Podczas korzystania z podstawowego uwierzytelniania ważne jest skorzystanie z połączenia SSL/TLS, aby zapewnić, że dane uwierzytelniające nie będą narażone na sniffing. `Ktor`, używany w naszym przykładzie, wspiera SSL.

## Zobacz też

1. [HTTP Basic Authentication - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
2. [Kotlin Ktor Library - GitHub](https://github.com/ktorio/ktor)
3. [Securing the transmission with SSL/TLS - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
4. [OAuth - Wikipedia](https://pl.wikipedia.org/wiki/OAuth)