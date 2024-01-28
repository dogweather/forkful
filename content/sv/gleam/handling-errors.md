---
title:                "Hantering av fel"
date:                  2024-01-26T00:52:38.779460-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hantering av fel"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Hantering av fel handlar om att förutse att saker kan gå fel i koden och hantera dessa situationer på ett smidigt sätt. Programmerare gör detta för att hålla applikationer robusta och användarvänliga, även när de stöter på det oväntade.

## Hur man gör:
I Gleam kommer du ofta att använda `Result`-typen för felsökning. Det är en uppräkning med två varianter: `Ok` (för framgång) och `Error` (för misslyckande). Här är ett enkelt exempel:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("Oops! Det gick sönder.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(meddelande) => {
      io.println(meddelande)
      0
    }
  }
}
```

Om du kör `main` med `might_fail(False)`, så returnerar den `42`. Om du skickar in `True`, skriver den ut "Oops! Det gick sönder." och returnerar `0`.

## Fördjupning
Gleams tillvägagångssätt för felsökning är influerat av dess Erlang-rötter. Historiskt sett använder Erlang en filosofi som går ut på "låt det krascha", som förlitar sig på övervakningsträd för att hantera processfel. Men när du skriver Gleam-kod som inte är inuti en process avsedd att övervakas, som inom en biblioteksfunktion, vill du hantera felen explicit.

Alternativ till att använda `Result` inkluderar att använda `Option`-typen för fall där något kan vara `None` (ingenting) eller `Some` (något), men dessa bär inte på felinformation. För att signalera fel över processgränser kan du använda Erlangs mekanismer för meddelandepassning.

Gleams felhantering speglar en funktionell programmeringsstil, där sidoeffekter (som fel) hanteras med typer och mönstermatchning, vilket ger klarhet och förutsägbarhet i hanteringen av fel.

## Se också
- [Erlangs felsökning](http://erlang.org/doc/reference_manual/errors.html)
