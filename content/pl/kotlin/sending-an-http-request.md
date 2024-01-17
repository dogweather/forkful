---
title:                "Wysyłanie żądania http"
html_title:           "Kotlin: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

Co to jest wysyłanie zapytań HTTP i dlaczego programiści to robią?

Wysyłanie zapytań HTTP jest procesem przesyłania żądań do serwera w celu uzyskania informacji lub wykonania określonej operacji. Programiści często korzystają z tego mechanizmu w swoich aplikacjach, ponieważ umożliwia on interakcję z zewnętrznymi źródłami danych, takimi jak API lub bazy danych.

Jak to zrobić:

```Kotlin
class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        
        // Utworzenie obiektu klasy URLRequest
        val url = URL("http://www.example.com")
        val urlConnection = url.openConnection() as HttpURLConnection
        // Ustawienie metody żądania HTTP i czasu oczekiwania
        urlConnection.requestMethod = "GET"
        urlConnection.readTimeout = 10000
        urlConnection.connectTimeout = 15000
        // Odczytanie danych z serwera i przypisanie ich do zmiennej
        val stream = urlConnection.inputStream
        val response = stream.bufferedReader().use { it.readText() }
        // Wyświetlenie odpowiedzi serwera w konsoli
        Log.d("Response from server", response)
    }
}

// Output:
// D/Response from server: example response
```

Wielka płyta:

Wysyłanie zapytań HTTP jest integralną częścią komunikacji w internecie od początku jego istnienia. Zamiast używać zwykłego protokołu, takiego jak TCP, przeglądarki internetowe oraz aplikacje mobilne i webowe korzystają z protokołu HTTP w celu uzyskania żądanych informacji lub danych.

Alternatywy:

Podczas wysyłania żądań HTTP w Kotlinie możemy użyć również bibliotek takich jak Retrofit, Volley czy Fuel. Pomagają one w uproszczeniu procesu tworzenia i obsługi zapytań.

Szczegóły implementacji:

Wysyłanie zapytań HTTP w Kotilnie możliwe jest dzięki wykorzystaniu klas jak HttpURLConnection lub OkHttpClient. Wykorzystują one metody takie jak requestMethod, readTimeout czy connectTimeout do konfiguracji żądań i ustalenia odpowiedniego czasu oczekiwania.

Zobacz również:

1. Dokumentacja Kotlin do klasy HttpURLConnection: https://developer.android.com/reference/java/net/HttpURLConnection
2. Porównanie bibliotek do obsługi zapytań HTTP w Kotlinie: https://futurestud.io/tutorials/retrofit-vs-volley-vs-fuel-for-th...
3. Przykładowe projekty wykorzystujące Kotlin do wysyłania zapytań HTTP: https://github.com/ligi/code_gen_move_to_kotlin#http-based-api-wi...