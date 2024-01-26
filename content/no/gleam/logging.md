---
title:                "Loggføring"
date:                  2024-01-26T01:04:10.053607-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggføring"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/logging.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Logging er i bunn og grunn hvordan vi registrerer det som foregår i programmene våre. Det er som å ha en liten svart boks; når ting går galt (og tro meg, det vil det), er logger uvurderlige for å finne ut hva som skjedde, diagnostisere problemer, og optimalisere ytelse.

## Hvordan gjøre:
I Gleam vil du typisk dra inn et logging-bibliotek – det er ingen dedikert loggingmekanisme rett ut av boksen. La oss si at vi bruker en hypotetisk `gleam_logger` crate. Slik kunne du integrere den:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("Appen starter opp!")
  let result = intense_calculation()

  case result {
    Ok(value) -> 
      gleam_logger.debug("Kalkulasjon vellykket", value)
    Error(err) -> 
      gleam_logger.error("Kalkulasjon feilet", err)
  }
}
```

Forventet utdata i loggene dine ville se noe slik ut:

```
INFO: Appen starter opp!
DEBUG: Kalkulasjon vellykket 42
ERROR: Kalkulasjon feilet Grunn: Deling på null
```

## Dypdykk
Kunsten å logge har vært rundt siden de tidlige dagene av programmering. Systemoperatører ville bokstavelig talt få logger fra datamaskinen - for å sikre at alt kjørte jevnt. Spol fremover, og logging har blitt digital, og er blitt en kjerne del av programvareutvikling.

Selv om Gleam, som et relativt ungt språk som målretter mot Erlang-økosystemet, ikke har et innebygd loggingrammeverk, kan du utnytte de modne Erlang-loggingfasilitetene eller andre biblioteker som tilbys av samfunnet. Hver har forskjellige funksjoner og kompromisser: noen kan tilby strukturert logging, andre er mer for enkel tekstutskrift.

Nå, spørsmålet om å implementere en loggingfasilitet: Er det enkelt? Ved første øyekast, ja. Men skrell tilbake lagene, og du ser på håndtering av parallellitet, I/O-flaskehalser, loggrotasjon, formatstandardisering (tenk JSON for strukturert logging), nivåfiltrering, og muligens distribuert sporing. Pluss, i et funksjonelt paradigme, ønsker du generelt at side-effekter (som logging) håndteres på en forutsigbar og kontrollert måte.

## Se Også
Her er hvor du kan finne mer om nøtter og bolter av logging i Gleam og det omliggende økosystemet:
- [Erlangs :logger dokumentasjon](http://erlang.org/doc/apps/kernel/logger_chapter.html): Siden Gleam kompilerer til Erlang, er dette direkte anvendelig.
- [Dokumentasjonen til Gleam sitt standardbibliotek](https://hexdocs.pm/gleam_stdlib/): For oppdateringer på eventuelle loggingverktøy som kan bli lagt til.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): En kuratert liste over ressurser, som kan inkludere loggingbiblioteker etter hvert som de blir tilgjengelige.