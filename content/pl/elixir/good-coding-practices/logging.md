---
title:                "Rejestrowanie zdarzeń"
date:                  2024-01-26T01:02:50.779259-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Logowanie w rozwoju oprogramowania to technika rejestrowania wydarzeń, które zachodzą podczas działania programu, zazwyczaj do pliku lub zewnętrznego systemu. Programiści stosują ją, aby zdobyć wgląd w zachowanie oprogramowania, rozwiązać problemy i utrzymać zapis historii operacyjnej, co jest kluczowe dla debugowania i monitorowania zdrowia aplikacji.

## Jak to zrobić:
W Elixirze, podstawowym sposobem logowania informacji jest wbudowany moduł `Logger`. Oto jak można go użyć:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Rozpoczynanie ważnego procesu z parametrem: #{param}")

    # Symulacja wykonywanej pracy
    :timer.sleep(1000)

    Logger.debug("Proces zakończony.")
  rescue
    error -> Logger.error("Wystąpił błąd: #{inspect(error)}")
  end
end

# Aby zobaczyć swoje logi, wystarczy wywołać funkcję:
MyApplication.do_something_important("MyParam")
```

Ten prosty fragment kodu pokazuje, jak logować na różnych poziomach (`info`, `debug` i `error`). Gdy uruchomisz ten kod, nie zobaczysz wiadomości debug, chyba że skonfigurujesz poziom Loggera na `:debug`. Domyślnie, Logger Elixira filtruje komunikaty logów poniżej poziomu `:info`.

Przykładowe wyjście na poziomie `:info` może wyglądać tak:
```
14:32:40.123 [info]  Rozpoczynanie ważnego procesu z parametrem: MyParam
14:32:41.126 [error] Wystąpił błąd: %RuntimeError{message: "błąd czasu wykonania"}
```

## Wgłębienie się:
`Logger` w Elixirze to wbudowane narzędzie, które jest częścią języka od jego wczesnych dni. Jest ono inspirowane systemami logowania z innych języków BEAM, takich jak Erlang. Logger dostarcza różne poziomy logowania – `:debug`, `:info`, `:warn` i `:error` – i jest rozszerzalny, co pozwala na podpięcie różnych backendów do obsługi komunikatów logów.

Alternatywą dla wbudowanego Loggera w bardziej złożonych scenariuszach jest użycie bibliotek logowania, takich jak `Logstash` lub `Sentry` dla Elixira, które mogą dostarczyć dodatkowe funkcje, takie jak śledzenie błędów i agregacja w bardziej wizualnym formacie. Do lokalnego rozwoju, deweloperzy Elixira często polegają na wbudowanej funkcjonalności Loggera ze względu na jego prostotę i integrację z maszyną wirtualną BEAM.

W modułach, Logger oferuje asynchroniczne i synchroniczne logowanie. Domyślne logowanie asynchroniczne nie blokuje wykonania aplikacji podczas rejestrowania komunikatów. Zapewnia to, że logowanie nie wpływa negatywnie na wydajność. Jednak logowanie synchroniczne może być włączone w przypadkach, gdy potrzebna jest gwarancja, że komunikaty są logowane w kolejności ich wysłania.

Konfiguracja Loggera może być dostosowana w pliku `config/config.exs` aplikacji Elixira, gdzie można ustawić poziom logowania, format, metadane i więcej. Zawsze pamiętaj, aby dostosować swoje poziomy logowania i wyjścia dla różnych środowisk; nie chciałbyś przecież, aby szczegółowe logi debugowania zalały twoje systemy produkcyjne.

## Zobacz również:
- Oficjalna dokumentacja Elixira Logger: https://hexdocs.pm/logger/Logger.html
- Blog post o najlepszych praktykach logowania w Elixirze: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry dla Elixira na Hex: https://hex.pm/packages/sentry
- Lekcja o Loggerze na Elixir School: https://elixirschool.com/en/lessons/specifics/debugging/#logging
