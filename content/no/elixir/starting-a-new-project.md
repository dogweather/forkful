---
title:                "Å starte et nytt prosjekt"
html_title:           "Elixir: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Når vi starter et nytt prosjekt, er det vanligvis for å utvikle en ny applikasjon eller forbedre en eksisterende en. Dette gjøres vanligvis av utviklere for å løse et spesifikt problem eller forbedre en prosess.

# Hvordan:

Et nytt Elixir-prosjekt kan opprettes ved å først installere Elixir-språket og deretter bruke kommandoen "mix new [prosjektnavn]". Dette vil opprette en mappe med prosjektnavnet og nødvendige filer for å kjøre en Elixir-applikasjon.

```elixir
mix new my_project
```

Etter å ha opprettet prosjektet, kan du navigere til mappen og åpne filen "lib/my_project.ex" for å begynne å skrive koden din.

```elixir
defmodule MyProject do
  # skriv koden din her
end
```

For å kjøre prosjektet, bruk "mix run [filnavn]" kommandoen i terminalen.

```elixir
mix run lib/my_project.ex
```

# Dypdykk:

Elixir er et funksjonelt programmeringsspråk som ble utgitt i 2011. Det er inspirert av språk som Ruby og Erlang, og er designet for å være skalerbart og robust for distribuerte systemer.

Et alternativ til å starte et nytt Elixir-prosjekt er å bruke en ferdiglaget ramme, eller "framework". Eksempler på populære Elixir-rammer er Phoenix og Ecto.

Når du starter et nytt prosjekt, vil mix kommandoen generere følgende mapper og filer: "config", "lib", "priv", "test", "mix.exs" og "mix.lock".

# Se også:

For mer informasjon om Elixir, kan du sjekke ut offisiell dokumentasjon [https://elixir-lang.org/].

For å lære mer om Phoenix-rammen, kan du besøke [https://phoenixframework.org/].