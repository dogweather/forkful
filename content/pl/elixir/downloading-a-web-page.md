---
title:                "Elixir: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Dlaczego warto pobrać stronę internetową przy użyciu Elixir?

Pobieranie stron internetowych jest nieodłączną częścią dzisiejszego świata internetu. Często potrzebujemy danych z różnych witryn, takich jak informacje o produktach, aktualności czy też ciekawostki. W tym celu można użyć języka programowania Elixir, który zapewnia wydajną i prostą w użyciu bibliotekę do pobierania stron internetowych.

# Jak tego dokonać?

Wykorzystując bibliotekę `HTTPoison` wraz z modułem `Floki`, możemy w prosty sposób pobrać stronę internetową. Poniżej znajduje się przykładowy kod, który pobiera stronę internetową i zwraca jej tytuł:

```Elixir
# Importowanie niezbędnych bibliotek
import HTTPoison
import Floki

# Pobranie strony internetowej
{:ok, response} = HTTPoison.get("https://www.example.com")

# Parsowanie strony przy użyciu Floki
parsed_page = Floki.parse(response.body)
# Pobranie tytułu strony
title = parsed_page |> Floki.find("title") |> Floki.text()

# Wypisanie tytułu
IO.puts("Tytuł strony to: #{title}")
```

Po uruchomieniu powyższego kodu, powinniśmy zobaczyć w konsoli następujący wynik:

```
Tytuł strony to: Example Domain
```

# Pogłębione informacje

Nie zawsze jednak pobieranie strony internetowej jest tak proste. Często witryny mają zabezpieczenia przeciwko botom i nie pozwalają na pobranie danych w prosty sposób. W takim przypadku musimy zastosować bardziej zaawansowane techniki, takie jak zmiana nagłówków żądań czy też użycie mechanizmu przekierowań.

Ponadto, możemy również wykorzystać Elixir do pobierania danych z witryn internetowych w tle, przy użyciu narzędzia `Task`. Dzięki temu nasz program będzie mógł wykonywać inne zadania, podczas gdy pobieranie strony jest w trakcie.

# Zobacz również

- [Dokumentacja biblioteki HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Dokumentacja modułu Floki](https://hexdocs.pm/floki/Floki.html)
- [Przykładowy projekt pobierający dane z Twittera przy użyciu Elixir](https://medium.com/@codedgeekery/web-scraping-with-elixir-examples-using-floki-and-hackney-c17df682b44b)