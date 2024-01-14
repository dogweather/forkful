---
title:                "Gleam: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Hvorfor

Du har kanskje hørt om det nye funksjonelle programmeringsspråket Gleam og lurer på om det er verdt å utforske. Vel, la meg fortelle deg hvorfor Gleam kan være et flott valg for ditt neste prosjekt.

Gleam kombinerer styrken til funksjonelle språk med en moderne og robust syntaks. Det er enkelt å lese og skrive, og det støtter samtidig ting som typeinferanse og mønstergjenkjenning. Dette gjør Gleam til et effektivt og robust alternativ for programmering, uansett om du er nybegynner eller en erfaren utvikler.

# Hvordan

For å komme i gang med Gleam, kan du følge disse enkle trinnene:

1. Installer Gleam ved å følge instruksjonene på deres offisielle nettside: https://gleam.run/getting-started/
2. Opprett en ny Gleam-fil. Du kan kalle den hva du vil, men la oss kalle den "hello_world.gleam" for vår demonstrasjon.
3. Skriv inn følgende kode i filen:

```
gleam
pub fn main() {
  let message = "Hei fra Gleam!";
  IO.println(message);
}
```

4. Kjør filen ved å kjøre følgende kommando i terminalen:

```
gleam run hello_world.gleam
```

5. Du vil se følgende output i terminalen:

```
Hei fra Gleam!
```

Gratulerer, du har akkurat laget og kjørt ditt første Gleam-program!

# Dypdykk

Nå som du har en grunnleggende forståelse av hvordan du kan kjøre Gleam-programmer, er det på tide å dykke dypere inn i detaljene om å starte et nytt prosjekt. Her er noen ekstra tips som kan være nyttige:

- Gleam er kompilert til Erlang-beam-kode og kjører på Erlang Virtual Machine (VM). Dette betyr at du kan dra nytte av det store økosystemet av biblioteker som allerede eksisterer for Erlang og Elixir.
- Gleam tilbyr også muligheten til å skrive interoperable moduler med Elixir (og snart også med andre språk som Detla eller Rust), noe som gjør det enda mer fleksibelt for større prosjekter.
- Det er en aktiv og voksende samfunn av Gleam-utviklere der ute, så ikke nøl med å stille spørsmål og søke etter hjelp hvis du støter på problemer.

# Se Også

- Offisiell Gleam nettside: https://gleam.run/
- Offisiell Dokumentasjon: https://gleam.run/getting-started/
- Gleam-utvikleres forum: https://elixirforum.com/c/gleam
- Gleam på Github: https://github.com/gleam-lang/gleam