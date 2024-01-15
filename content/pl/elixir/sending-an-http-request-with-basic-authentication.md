---
title:                "Wysyłanie żądania http z uwierzytelnianiem podstawowym"
html_title:           "Elixir: Wysyłanie żądania http z uwierzytelnianiem podstawowym"
simple_title:         "Wysyłanie żądania http z uwierzytelnianiem podstawowym"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

### Dlaczego

Jeśli pracujesz z siecią lub chcesz zapewnić bezpieczeństwo swoim aplikacjom, prawdopodobnie już znasz podstawy obsługi żądań HTTP. Ale co z autoryzacją? Czasami musisz wysłać żądanie z podanymi danymi uwierzytelniającymi, a tutaj wkracza autoryzacja podstawowa. W tym artykule dowiesz się, dlaczego i jak wysłać żądanie HTTP z autoryzacją podstawową w Elixir.

### Jak To Zrobić

W Elixir, aby wysłać żądanie HTTP z autoryzacją, musisz użyć modułu `HTTPoison` z biblioteki `HTTPoison`. Najpierw musisz dodać to do swojego projektu, korzystając z menedżera pakietów `mix`:
```elixir
defp deps do
 [{:httpoison, "~> 1.8"}]
end
```

Następnie musisz zaimportować moduł `HTTPoison` w swoim pliku kodu:
```elixir
import HTTPoison
```

Teraz możesz wysłać żądanie HTTP z autoryzacją podstawową, dodając odpowiednie nagłówki, używając funkcji `request/4` z `HTTPoison`. Najpierw musisz utworzyć nagłówek `Authorization` zawierający kodowanie base64 twojego loginu i hasła, oddzielone dwukropkiem (`:`):
```elixir
auth_header = "Basic " <> Base.encode64("username:password")
```

Następnie, możesz wysłać żądanie z autoryzacją, podając metodę (np. `:get`), URL, nagłówki, w tym nagłówek autoryzacyjny, i ciało żądania:
```elixir
response = HTTPoison.request(:get, "https://api.example.com/users", [authorization: auth_header], %{name: "John", age: 30})
```

Pamiętaj, że login i hasło powinny być przekazane w formacie `username:password`, a następnie zakodowane przy użyciu base64.

#### Przykładowe Wyjście

Jeśli wszystko poszło dobrze, dostaniesz odpowiedź z serwera z kodem odpowiedzi oraz ciałem, które powinno zawierać użytkowników o imieniu "John" i wieku 30. Aby to zweryfikować, możesz wyświetlić w konsoli kod odpowiedzi i ciało odpowiedzi:
```elixir
IO.inspect response.status_code # powinno zwrócić 200
IO.inspect response.body # powinno zwrócić [{name: "John", age: 30}, ...]
```

### Deep Dive

Autoryzacja podstawowa jest jedną z wielu metod autoryzacji w protokole HTTP. Jest ona prosta do zaimplementowania i jest szeroko wykorzystywana w aplikacjach internetowych. Polega na wysłaniu kodowanego base64 loginu i hasła w nagłówku `Authorization`. Serwer następnie sprawdza autoryzację, porównując te dane z bazą użytkowników lub innymi źródłami autoryzacji.

Jedną z głównych zalet autoryzacji podstawowej jest jej prostota, ale jest ona również jedną z jej głównych wad. Ponieważ dane autoryzacyjne są przesyłane jako tekst jawny, mogą być łatwo przechwycone przez niepożądane osoby. W takich przypadkach zaleca się stosowanie bardziej bezpiecznych metod autoryzacji, takich jak OAuth.

### Zobacz Również

Jeśli chcesz pogłębić swoją wiedzę o autoryzacji w Elixir, polecamy poznanie innych sposobów autoryzacji, takich jak OAuth czy autoryzacja tokenowa, a także zapoznanie się z innymi funkcjonalnościami modułu `HTTPoison`, takimi jak obsługa błędów i przekierowań