---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Kotlin: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Cześć programiści! Dzisiaj porozmawiamy o tym, jak wysyłać żądania HTTP z podstawowym uwierzytelnieniem w Kotlinie. To nie jest trudne, więc nie będziemy się za dużo rozpisywać. Zacznijmy!

## Co to jest i po co to robimy?

Wysyłanie żądań HTTP z podstawowym uwierzytelnieniem to po prostu wysyłanie informacji do serwera internetowego wraz z kodem uwierzytelniającym, który jest wymagany do autoryzacji. Programiści robią to, gdy chcą uzyskać dostęp do chronionej przez hasło zawartości lub usługi, na przykład w przypadku aplikacji mobilnych czy stron internetowych z logowaniem.

## Jak to zrobić:

Kotlin jest jednym z języków programowania, które dobrze wspierają wysyłanie żądań HTTP z podstawowym uwierzytelnieniem. Oto przykładowy kod:

```Kotlin
val url = "adres_url"
val username = "login"
val password = "hasło"

val client = OkHttpClient()
val request = Request.Builder()
    .url(url)
    .addHeader("Authorization", Credentials.basic(username, password))
    .build()

val response = client.newCall(request).execute()
println(response.body()?.string())
```

Bardzo ważne jest, aby znać adres URL, login i hasło, które są wymagane do autoryzacji. Następnie tworzymy obiekt HttpClient i ustawiamy nagłówek Authorization z użyciem metody Credentials.basic, wykorzystując nasze dane logowania. W końcu wykonujemy żądanie i pobieramy odpowiedź, którą drukujemy w konsoli.

## Deep Dive:

Wysyłanie żądań HTTP z podstawowym uwierzytelnieniem jest jednym z sposobów na autoryzację żądań do serwera. Alternatywą może być na przykład OAuth, który jest bardziej bezpiecznym i elastycznym rozwiązaniem dla uwierzytelniania. Implementacja metody Credentials.basic jest oparta na nagłówku Basic Access Authentication, który jest nieważny bez szyfrowania HTTPS.

## Zobacz także:

Jeśli chcesz dowiedzieć się więcej o tym, jak wysyłać żądania HTTP z podstawowym uwierzytelnieniem w Kotlinie, zapoznaj się z dokumentacją języka oraz z zasobami dostępnymi online. Powodzenia w pisaniu bezpiecznych i skutecznych aplikacji!