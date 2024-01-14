---
title:    "Gleam: Skriva till standardfel"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

Ibland kan det hända att vi behöver lite extra information eller feedback från våra program när vi kör dem i terminalen. Att skicka ut det till standard error är ett sätt att göra det. Det kan hjälpa oss att felsöka och förstå vad som händer bakom kulisserna.

## Så här gör du

För att skicka information till standard error i Gleam använder vi funktionen `erlang:error/1` tillsammans med en formaterad sträng. Detta är ett exempel på hur vi kan göra det:

```Gleam
error_msg = "Det här är ett felmeddelande"
erlang:error(error_msg)
```

Detta resulterar i följande utmatning när vi kör vårt program:

```
=ERROR REPORT==== 1-jan-2021::12:34:56.789 ===
Användardefinierat fel: "Det här är ett felmeddelande"
```

Som vi kan se skriver Erlang ut ett standardformat för felrapporter och sedan vår egen sträng som vi skickade till `erlang:error/1`.

## Djupdykning

Nu när vi vet hur vi skriver till standard error, låt oss titta lite närmare på funktionen `erlang:error/1`. Denna funktion tar en sträng som argument och skapar ett felobjekt som sedan skickas till standard error. Vi kan även använda den för att kasta och hantera våra egna fel i våra Gleam-program.

Det finns också andra sätt att skicka information till standard error i Gleam, som t.ex. att använda `erlang:error/2` för att inkludera en stacktrace tillsammans med vårt felmeddelande. Mer information om detta finns i dokumentationen för Erlang.

## Se även

- [Dokumentation för Erlang error-funktionen](https://erlang.org/doc/man/erlang.html#error-1)
- [Mer information om felhantering i Gleam](https://gleam.run/book/the-gleam-programming-language-error-handling.html)