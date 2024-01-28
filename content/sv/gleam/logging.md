---
title:                "Loggning"
date:                  2024-01-26T01:04:32.798562-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggning"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/logging.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Loggning är i grund och botten hur vi dokumenterar vad som händer i våra program. Det är som att ha en liten svart låda; när saker går fel (och lita på mig, det kommer de att göra), är loggar ovärderliga för att ta reda på vad som hände, diagnostisera problem och optimera prestanda.

## Hur man gör:
I Gleam skulle du vanligtvis ta in ett loggningsbibliotek – det finns ingen dedikerad loggningsmekanism direkt ur lådan. Låt oss säga att vi använder en hypotetisk `gleam_logger` crate. Så här kan du integrera den:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("Appen startar!")
  let resultat = intensiv_beräkning()

  case resultat {
    Ok(värde) -> 
      gleam_logger.debug("Beräkning lyckades", värde)
    Error(err) -> 
      gleam_logger.error("Beräkning misslyckades", err)
  }
}
```

Förväntad utmatning i dina loggar skulle se ut ungefär så här:

```
INFO: Appen startar!
DEBUG: Beräkning lyckades 42
ERROR: Beräkning misslyckades Anledning: Delning med noll
```

## Fördjupning
Konsten att logga har funnits sedan programmeringens tidiga dagar. Systemoperatörer skulle bokstavligen få loggar från datorn - för att se till att allt gick smidigt. Hoppa fram till idag, och loggningen har blivit digital, och är en kärnkomponent i mjukvaruutveckling.

Även om Gleam, som är ett relativt ungt språk som riktar sig mot Erlang-ekosystemet, inte har ett inbyggt loggningsramverk, kan du dra fördel av de mogna loggningsfaciliteterna i Erlang eller andra bibliotek som tillhandahålls av communityn. Varje har olika funktioner och avvägningar: några kan tillhandahålla strukturerad loggning, andra är mer för enkel textutmatning.

Nu till frågan om att implementera en loggningsfacilitet: Är det enkelt? Vid första anblick, ja. Men skrapa på ytan och du ser att det handlar om att hantera konkurrens, I/O-flaskhalsar, loggrotation, standardisering av format (tänk JSON för strukturerad loggning), filtrering av nivå och möjligtvis distribuerad spårning. Dessutom vill du i ett funktionellt paradigm generellt ha sidoeffekter (som loggning) hanterade på ett förutsägbart och kontrollerat sätt.

## Se även
Här kan du hitta mer om detaljerna i loggning i Gleam och dess omgivande ekosystem:
- [Erlangs :logger-dokumentation](http://erlang.org/doc/apps/kernel/logger_chapter.html): Eftersom Gleam kompileras till Erlang, är detta direkt tillämpligt.
- [Gleams standardbiblioteksdokumentation](https://hexdocs.pm/gleam_stdlib/): För uppdateringar om eventuella loggningsverktyg som kan läggas till.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): En kurerad lista över resurser, vilken kan inkludera loggningsbibliotek när de blir tillgängliga.
