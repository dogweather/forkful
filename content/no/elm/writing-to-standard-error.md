---
title:                "Elm: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

I dette innlegget skal vi dykke inn i hvordan man skriver til standard error i Elm. Dette kan være nyttig hvis du ønsker å logge feil eller varsler i din applikasjon. Det kan også hjelpe deg med å løse potensielle problemer som oppstår under utviklingsprosessen. La oss se på hvorfor dette kan være en god teknikk å bruke.

## Hvordan

Det første du må gjøre er å importere `Task`-modulen i ditt prosjekt. `Task`-modulen lar oss utføre asynkrone oppgaver, som å skrive til standard error. Ved å bruke funksjonen `perform`, kan du starte en asynkron oppgave og gi den en funksjon som skal utføres.

```Elm
import Task exposing (..)

-- Definer en funksjon for å skrive til standard error
writeToStdErr : String -> Tack Never ()
writeToStdErr message =
  perform (succeed (Debug.log "Feilmelding" message))
```

Vi bruker `Debug.log`-funksjonen for å logge meldingen vår til standard error. Du kan erstatte denne med din egen loggefunksjon, eller bare bruke `Debug.log` som den er.

For å kjøre denne funksjonen, kan vi bruke `perform` inne i en annen funksjon. For eksempel:

```
-- En funksjon som dobler et tall
double : Int -> Int
double n =
  2 * n

main : Program () Model Msg
main =
  Browser.sandbox { init = 0, update = update, view = view }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Double ->
      (double model, Task.succeed ())
    LogError ->
      (model, writeToStdErr "En feil har oppstått")
```

Som du kan se, kan vi bruke `writeToStdErr`-funksjonen i en `Cmd` for å logge en feilmelding når `LogError`-meldingen blir sendt.

## Deep Dive

Nå som vi forstår hvordan vi kan skrive til standard error, la oss dykke litt dypere inn i emnet. Det finnes også en annen nyttig funksjon i `Task`-modulen som lar oss logge til standard error: `synchronously`.

Forskjellen mellom `synchronously` og `perform` er at `perform` kjører asynkront, mens `synchronously` kjører synkront. Dette betyr at koden din vil vente på at `synchronously`-funksjonen er ferdig før den fortsetter å kjøre. Dette kan være nyttig hvis du trenger å logge en feilmelding før du fortsetter med neste handling.

```Elm
-- En funksjon som dobler et tall og logger verdien til standard error
doubleAndLog : Int -> Tack Never ()
doubleAndLog n =
  perform (succeed (Debug.log "Resultat" (2 * n)))

-- En funksjon som dobler et tall og logger verdien synkront til standard error
doubleAndLogSync : Int -> Tack Never ()
doubleAndLogSync n =
  synchronously (Debug.log "Resultat" (2 * n))
```

Som du kan se er koden ganske lik, men `doubleAndLogSync` bruker `synchronously` i stedet for `perform` for å logge verdien. Dette kan komme godt med når du trenger å logge noe på et spesifikt tidspunkt i koden din.

## Se også

- Offisiell Elm dokumentasjon om Task-modulen: https://package.elm-lang.org/packages/elm/core/latest/Task