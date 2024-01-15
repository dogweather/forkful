---
title:                "Przesyłanie żądania http"
html_title:           "Kotlin: Przesyłanie żądania http"
simple_title:         "Przesyłanie żądania http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele powodów, dla których możesz wchodzić w interakcje z zasobami sieciowymi, ale jeden z najczęstszych to pobieranie danych z zewnętrznych serwerów. Przykładowo, możesz chcieć pobrać informacje o pogodzie z API pogodowego lub dane użytkownika z zewnętrznej bazy danych. Do tego potrzebny jest wysyłanie żądań HTTP.

## Jak to zrobić

Aby wysłać żądanie HTTP w języku Kotlin, możesz skorzystać z biblioteki Retrofit. Najpierw musisz dodatkowo dodać bibliotekę do swojego projektu, na przykład poprzez dodanie jej do pliku Gradle. Następnie, musisz zdefiniować interfejs z metodami, które odpowiadają żądanym operacjom HTTP. Podajemy przykładowy interfejs i przykładowe użycie w kodzie Kotlin:

```Kotlin
// Definicja interfejsu
interface WeatherApi {
    @GET("weather")
    fun getWeather(
        @Query("city") city: String,
        @Query("units") units: String
    ): Call<WeatherResponse>
}

// Tworzenie obiektu serwisu
val retrofit = Retrofit.Builder()
    .baseUrl("https://api.openweathermap.org/data/2.5/")
    .addConverterFactory(GsonConverterFactory.create())
    .build()

// Wywołanie żądania
val service = retrofit.create(WeatherApi::class.java)
val call = service.getWeather("Warsaw", "metric")
val response = call.execute()
```

W przykładzie powyżej użyto metody `GET`, ale można również używać innych metod HTTP, takich jak `POST`, `PUT` czy `DELETE`. Możesz również przekazywać parametry w różnych formatach, np. jako ścieżkę lub zapytanie (`@Path` lub `@Query`). Dodatkowo, biblioteka Retrofit umożliwia łatwe przetwarzanie danych, dzięki zastosowaniu konwerterów, w tym w tym przypadku formatu JSON.

Wynik żądania można odczytać na kilka sposobów, na przykład można skorzystać z biblioteki Gson, aby przekonwertować dane na obiekty. Przykładowo, w powyższym kodzie użyto metody `execute()`, aby otrzymać odpowiedź jako obiekt `WeatherResponse`.

## Głębsze zagłębienie

Wysyłanie żądań HTTP jest szerokim zagadnieniem, które można badać na wiele sposobów. Przedstawiony powyżej przykład jest tylko jednym z możliwych podejść i tylko we fragmentaryczny sposób dotyka tematu. Warto zapoznać się z innymi bibliotekami, takimi jak okHttp czy ktor, aby porównać ich funkcjonalność i wybrać najlepsze rozwiązanie dla swojego projektu.

## Zobacz także

- Dokumentacja Retrofit: https://square.github.io/retrofit/
- Dokumentacja Gson: https://github.com/google/gson
- Porównanie bibliotek HTTP w języku Kotlin: https://medium.com/@mayankwhat/okhttp-vs-ktor-vs-retrofit-which-networking-library-i-should-use-for-android-app-b7358897286e