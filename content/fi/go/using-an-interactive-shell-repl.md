---
title:                "Interaktiivisen komentotulkin (REPL) käyttö"
date:                  2024-01-26T04:14:39.132718-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interaktiivisen komentotulkin (REPL) käyttö"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
REPL (Read-Eval-Print Loop) mahdollistaa koodin reaaliaikaisen interaktion; se lukee syötteen, arvioi sen, tulostaa tuloksen ja aloittaa alusta. Ohjelmoijat käyttävät sitä pätkien testaamiseen, virheenkorjaukseen ja uusien kielien oppimiseen reaaliajassa.

## Kuinka:
Go ei sisällä sisäänrakennettua REPL:iä, mutta voit käyttää kolmannen osapuolen työkaluja. Yksi suosittu työkalu on `gore`:

```go
// Asenna gore käyttäen
$ go install github.com/motemen/gore/cmd/gore@latest

// Käynnistä gore
$ gore
gore versio 0.5.0  :help apua varten
gore> :import fmt
gore> fmt.Println("Hei, Go REPL!")
Hei, Go REPL!
nil
```

## Syväsukellus
Alun perin Lispille kehitetyt REPL:t ovat yleisiä dynaamisissa kielissä, kuten Python tai Ruby. Go, ollessaan staattisesti tyypitetty, ei sisällä sellaista suoraan. Vaihtoehtoja `gore`:lle ovat `go-pry` ja `yaegi`. Nämä työkalut tulkitsevat Go-koodia, antaen sinun tutkia ja validoida ideoita nopeasti ilman täysimittaisen sovelluksen kääntämistä. Ne ovat erityisen hyödyllisiä aloittelijoille ja koulutusyhteyksissä, joissa keskitytään oppimiseen ja kokeiluun.

## Katso Myös
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
