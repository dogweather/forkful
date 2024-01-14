---
title:                "Kotlin: Wysyłanie żądania http z podstawową autoryzacją."
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Dlaczego

W dzisiejszych czasach przesyłanie danych przez internet stało się powszechne, a więc potrzebujemy sposobu, aby te informacje były bezpieczne. W tym artykule pokażę Ci, jak wysyłać żądania HTTP z podstawowym uwierzytelnianiem w języku Kotlin.

# Jak to zrobić

Aby użyć podstawowego uwierzytelniania w żądaniach HTTP, musimy umieścić odpowiednie nagłówki w naszym kodzie. W poniższym przykładzie używamy biblioteki `OkHttp` w celu wysłania żądania GET z uwierzytelnieniem typu Basic.

```Kotlin
val client = OkHttpClient()

val request = Request.Builder()
    .url("https://example.com")
    .header("Authorization", Credentials.basic("username", "password"))
    .build()

val response = client.newCall(request).execute()

println(response.body?.string())
```

W powyższym przykładzie używamy metody `basic()` z klasy `Credentials` w celu stworzenia nagłówka uwierzytelniającego. Ten nagłówek jest następnie dodany do naszego żądania poprzez metodę `header()`. Po otrzymaniu odpowiedzi, możemy wypisać treść odpowiedzi na konsoli.

# Deep Dive

Uwierzytelnienie typu Basic jest jednym z najprostszych sposobów na zabezpieczenie naszych żądań HTTP. Polega ono na przesłaniu użytkownika i hasła za pomocą nagłówka `Authorization` w formacie `username:password` zakodowanym w Base64.

Istnieje wiele innych metod uwierzytelniania, takich jak OAuth czy JWT, jednak podstawowe uwierzytelnienie jest wciąż szeroko stosowane w wielu aplikacjach internetowych.

# Zobacz też

- Oficjalna dokumentacja OkHttp: https://square.github.io/okhttp/
- Więcej informacji o uwierzytelnianiu typu Basic: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication