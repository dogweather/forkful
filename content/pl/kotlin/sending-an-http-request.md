---
title:                "Kotlin: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Dlaczego warto używać wysyłania żądań HTTP w Kotlinie?

Wysyłanie żądań HTTP (Hypertext Transfer Protocol) jest niezwykle ważnym aspektem w programowaniu w języku Kotlin. Pozwala to na komunikację między aplikacją a serwerem, co jest niezbędne w dzisiejszych aplikacjach internetowych. W tym artykule przedstawimy krótkie wprowadzenie do wysyłania żądań HTTP w Kotlinie, aby pokazać, dlaczego jest to takie ważne dla każdego programisty.

## Jak to zrobić?

Aby wysłać żądanie HTTP w Kotlinie, potrzebujemy użyć biblioteki `OkHttp`. Musimy też dodać odpowiednie zależności do naszego projektu w pliku `build.gradle`. Następnie, w celu wysłania żądania, używamy metody `newCall()` w bibliotece `OkHttp` i wybieramy typ żądania (GET, POST, itp.). Na przykład:

```Kotlin
val request = Request.Builder()
                .url("http://www.example.com")
                .build()

val client = OkHttpClient()
val call = client.newCall(request)
```

W tym przykładzie tworzymy obiekt żądania z podanym adresem URL i budujemy go. Następnie, tworzymy nowy obiekt `OkHttpClient` i używamy go do wysłania żądania przy pomocy metody `newCall()`. Teraz pozostaje tylko wykonać żądanie i obsłużyć odpowiedź:

```Kotlin
call.enqueue(object : Callback {
    override fun onFailure(call: Call, e: IOException) {
        // obsługa błędu
    }

    override fun onResponse(call: Call, response: Response) {
        val responseBody = response.body()?.string()
        // obsługa odpowiedzi
    }
})
```

W przypadku powodzenia żądania, wywoływana jest metoda `onResponse()`, która przyjmuje jako parametr obiekt `Response`. Dzięki temu możemy uzyskać dostęp do ciała odpowiedzi i obsłużyć je w odpowiedni sposób. W przypadku błędu, metoda `onFailure()` jest wywoływana, gdzie możemy obsłużyć wyjątek.

## Głębszy zanurzenie

Wysłanie żądania HTTP to proces nieco bardziej skomplikowany niż tylko jedna metoda. Wymaga także uwzględnienia różnych kodów odpowiedzi HTTP, zawierających informacje na temat sukcesu lub błędu wykonania żądania. Wysyłanie żądań wymaga również dodatkowych metod, takich jak `addHeader()` do dodawania nagłówków do żądania, lub `body()` do umieszczenia danych w ciele żądania. Istnieje także możliwość uwierzytelnienia przy pomocy `Authenticator` w przypadku, gdy nasze żądanie wymaga autoryzacji.

Wysyłanie żądań HTTP jest niezwykle ważnym aspektem tworzenia aplikacji internetowych. Dzięki temu jesteśmy w stanie komunikować się z serwerem i wymieniać informacje pomiędzy aplikacją a użytkownikiem. W artykule skupiliśmy się na podstawach wysyłania żądań HTTP w Kotlinie, jednak istnieje znacznie więcej możliwości i metod, które mogą być wykorzystywane w zależności od naszych potrzeb.

## Zobacz także

- Dokumentacja biblioteki OkHttp: https://square.github.io/okhttp/
- Wysyłanie żądań w Kotlinie - przykłady i wyjaśnienia: https://www.geeksforgeeks.org/sending-http-request-in-kotlin/
- Wstęp do protokołu HTTP: https://developer.mozilla.org/pl/docs/Web/HTTP/Overview