---
title:                "Korzystanie z debugera"
date:                  2024-01-26T03:49:46.040100-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z debugera"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Użycie debugera to tak, jakbyś stał się detektywem w swoim kodzie, polując na błędy i rozgryzając, dlaczego coś nie działa płynnie. Programiści robią to, ponieważ, spójrzmy prawdzie w oczy, błędy są nieuniknione, a ich skuteczne eliminowanie oznacza szybsze i bardziej niezawodne uruchamianie kodu.

## Jak to zrobić:
Gleam obecnie opiera się na ekosystemie Erlanga pod względem narzędzi, więc zwykle debugujesz za pomocą narzędzi takich jak `rebar3`, `observer` i `debugger`. Oto jak zacząć pracę z debugowaniem:

```gleam
// W swojej konfiguracji rebara upewnij się, że masz te linie, aby dołączyć informacje debugowania:
{erl_opts, [debug_info]}.

// Uruchom powłokę Erlanga z załadowaną aplikacją
rebar3 shell

// W powłoce możesz uruchomić debugera
1> debugger:start().
```

Proste, prawda? Pojawi się GUI debugera, i możesz ustawiać punkty przerwania, przejść przez kod krok po kroku i obserwować zmienne do woli. Nie zobaczysz bezpośrednio kodu Gleam, ale kod Erlanga, do którego jest kompilowany, co jest wciąż dość pomocne.

## Głębsze spojrzenie
Gleam jest młodym językiem, więc mimo że opiera się na ekosystemie Erlanga, narzędzia do debugowania specyficzne dla Gleam nie są jeszcze na pierwszym planie. Oznacza to, że korzystamy ze sprawdzonych narzędzi Erlanga, i to nie jest nic złego. Debugger Erlanga istnieje od lat 90., doskonalony przez lata eliminowania irytujących błędów w systemach, gdzie niezawodność jest kluczowa.

Jeśli chodzi o alternatywy, śledzenie jest potężną metodą w świecie BEAM (to wirtualna maszyna, na której działa kod Erlanga i Elixira). Korzystając z `rebar3`, możesz uzyskać dostęp do narzędzi takich jak `recon`, aby śledzić wywołania funkcji i zagłębiać się w problemy związane z wydajnością.

Przełączanie się między pisaniem w Gleam a debugowaniem w Erlangu może wydawać się, jakbyś na bieżąco tłumaczył swoje myśli. Ale zaleta jest taka, że możesz zerknąć do świata Erlanga, rozumiejąc budulce swojej aplikacji w jej formie uruchomieniowej.

## Zobacz także
Aby rozszerzyć swój zestaw narzędzi do debugowania, sprawdź:

- Dokumentację debugera Erlanga: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- Bibliotekę `recon` dla Erlanga: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
- O śledzeniu w BEAM: [https://adoptingerlang.org/docs/development/tracing/](https://adoptingerlang.org/docs/development/tracing/)