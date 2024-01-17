---
title:                "Utskrift av feilrettingsutgang"
html_title:           "Go: Utskrift av feilrettingsutgang"
simple_title:         "Utskrift av feilrettingsutgang"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Printing av feilsøkingsutgang er en vanlig praksis blant programmører for å hjelpe dem med å identifisere feil og feilkilder i koden sin. Dette gjøres ved å inkludere utskriftsmeldinger i koden som viser verdier av variable og andre relevante data mens programmet kjører.

# Hvordan:
`` `Go
fmt.Printf("Verdi av variabelen er % d", variabel)
`` `
Eksempel på utdata:
` Verdi av variabelen er 10 `
Dette vil skrive ut verdien av variabelen "variabel" til konsollen mens programmet kjører.

# Dypdykk
Mange programmeringsspråk har innebygde funksjoner for å skrive ut feilsøkingsinformasjon, som "print" -funksjonen i Python. I Go bruker vi "fmt" pakken for å skrive ut utgang til konsollen. Det er også mulig å logge feilsøkingsmeldinger til en fil ved hjelp av "log" -pakken i Go.

# Se også
- https://golang.org/pkg/fmt/
- https://golang.org/pkg/log/