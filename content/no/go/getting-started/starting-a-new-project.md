---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:29.983848-07:00
description: "Hvordan: F\xF8rst, s\xF8rg for at du har Go installert ved \xE5 kj\xF8\
  re `go version` i terminalen din. Du b\xF8r se versjonen av Go du har installert\
  \ som utdata.\u2026"
lastmod: '2024-03-13T22:44:40.267632-06:00'
model: gpt-4-0125-preview
summary: "F\xF8rst, s\xF8rg for at du har Go installert ved \xE5 kj\xF8re `go version`\
  \ i terminalen din."
title: Starter et nytt prosjekt
weight: 1
---

## Hvordan:
Først, sørg for at du har Go installert ved å kjøre `go version` i terminalen din. Du bør se versjonen av Go du har installert som utdata. Deretter, la oss starte et nytt prosjekt. Naviger til ditt arbeidsområde og kjør:

```shell
mkdir hello-world
cd hello-world
```

Dette oppretter og flytter deg inn i en ny mappe for prosjektet ditt. Nå, initialiser modulen:

```shell
go mod init example.com/hello-world
```

Erstatt `example.com/hello-world` med din modulbane. Denne kommandoen lager en `go.mod`-fil i mappen din, som signaliserer starten på en ny Go-modul. Slik kan `go.mod` se ut:

```plaintext
module example.com/hello-world

go 1.18
```

`go.mod` spor prosjektets avhengigheter. Nå, opprett en `main.go`-fil:

```shell
touch main.go
```

Åpne `main.go` i din favorittredigerer og legg til følgende kode for å skrive ut "Hello, World!":

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

For å kjøre programmet ditt, naviger tilbake til terminalen og utfør:

```shell
go run main.go
```

Du bør se:

```plaintext
Hello, World!
```

Gratulerer! Du har nettopp startet et nytt Go-prosjekt og kjørt ditt første Go-program.

## Dypdykk
Initiativet til å innføre moduler som standarden for avhengighetsstyring i Go var et betydelig skifte i Go-økosystemet, offisielt vedtatt i Go 1.11. Før moduler, stolte Go-utviklere på GOPATH-miljøvariabelen for å håndtere avhengigheter, noe som var mindre intuitivt og ofte ledet til den beryktede "avhengighetshelvete."

Moduler tilbyr en innesluttet måte å håndtere prosjektavhengigheter, versjonering på, og er et skritt mot å gjøre Go-prosjekter mer selvforsynte og bærbare. Hver modul spesifiserer sine avhengigheter som Go sporer i `go.mod`-filen, noe som forenkler håndteringen av avhengigheter på tvers av ulike miljøer og utviklingsstadier.

Det er imidlertid verdt å merke seg at selv om Go-moduler nå er standarden, kan noen eldre prosjekter fortsatt bruke GOPATH. For de fleste nye prosjekter tilbyr moduler et mer greit og effektivt styringssystem, men å forstå GOPATH kan være nyttig for å vedlikeholde eller bidra til eldre Go-kodebaser.

Når det gjelder alternativer, selv om Go-moduler nå er de facto standarden, har Go-samfunnet eksperimentert med andre avhengighetsstyringsverktøy som `dep` i fortiden. Disse har imidlertid stort sett blitt erstattet av den offisielle modulstøtten integrert i Go-verktøykjeden.
