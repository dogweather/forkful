---
title:                "Wysłanie żądania HTTP z uwierzytelnieniem podstawowym"
html_title:           "Kotlin: Wysłanie żądania HTTP z uwierzytelnieniem podstawowym"
simple_title:         "Wysłanie żądania HTTP z uwierzytelnieniem podstawowym"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie zapytania HTTP z uwierzytelnieniem podstawowym jest ważną umiejętnością dla programistów, którzy chcą mieć kontrolę nad dostępem do swoich aplikacji lub serwisów sieciowych. Dzięki temu rodzajowi uwierzytelnienia, programiści mogą zapewnić bezpieczeństwo swoim użytkownikom i kontrolować dostęp do zasobów w aplikacji.

## Jak to zrobić

Aby wysłać zapytanie HTTP z uwierzytelnieniem podstawowym w języku Kotlin, wystarczy użyć biblioteki `OkHttp`. W poniższym przykładzie pokazano, jak dodać nagłówek `Authorization` z danymi uwierzytelniającymi do zapytania.

```Kotlin 
val client = OkHttpClient()
val username = "login"
val password = "hasło"
val credential = "$username:$password".toByteArray().encodeBase64()
val request = Request.Builder()
  .url("https://example.com/api/data")
  .addHeader("Authorization", "Basic $credential")
  .build()

val response = client.newCall(request).execute()
println(response.body?.string())
```

W powyższym kodzie, najpierw tworzymy obiekt klienta `OkHttpClient`, a następnie definiujemy nasze dane uwierzytelniające - login i hasło. Następnie, przy użyciu metody `encodeBase64()` konwertujemy dane do odpowiedniego formatu, aby mogły zostać przesłane w nagłówku `Authorization`. W końcu, tworzymy obiekt zapytania z odpowiednim adresem url i dodajemy nagłówek `Authorization` z uwierzytelnieniem podstawowym. Po wykonaniu zapytania, możemy przeczytać odpowiedź i wyświetlić jej zawartość.

## Deep Dive

Warto wiedzieć, że uwierzytelnienie podstawowe jest jednym z najprostszych sposobów uwierzytelniania w protokole HTTP. Polega ono na przesłaniu danych uwierzytelniających w nagłówku `Authorization` w postaci loginu i hasła, zakodowanych w formacie Base64. Jednak ze względu na to, że dane są przesyłane w formacie niezaszyfrowanym, nie jest to zalecane dla aplikacji, które wymagają wysokiego poziomu bezpieczeństwa.

W przypadku, gdy aplikacja wymaga wyższego poziomu bezpieczeństwa, zalecane jest użycie uwierzytelnienia tokenowego lub JWT (JSON Web Token). Jest to bardziej zaawansowana metoda uwierzytelniania, która wymaga wygenerowania i przesłania tokena uwierzytelniającego zamiast loginu i hasła.

## See Also

1. Dokumentacja biblioteki `OkHttp`: https://square.github.io/okhttp/
2. Więcej informacji o uwierzytelnieniu podstawowym w protokole HTTP: https://developer.mozilla.org/pl/docs/Web/HTTP/Authentication