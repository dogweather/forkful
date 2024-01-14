---
title:                "Elixir: Sänd en http-förfrågan med grundläggande autentisering"
simple_title:         "Sänd en http-förfrågan med grundläggande autentisering"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Att skicka en HTTP-förfrågan med grundläggande autentisering är en vanlig uppgift inom webbutveckling. Det kan användas för att skydda känslig information eller för att begränsa åtkomst till vissa resurser på en server. I denna artikel, kommer vi att titta närmare på hur man kan implementera detta med Elixir programmeringsspråk.

## Så här gör du
För att skicka en HTTP-förfrågan med grundläggande autentisering i Elixir, behöver vi använda oss av en modul som heter ```HTTPoison```. Det är en HTTP-klient som gör det enkelt att göra förfrågningar och få svar från externa webbsidor.

För att börja, måste vi importera ```HTTPoison``` modulen och definiera en bas URL för den webbplats som vi vill skicka en förfrågan till. Sedan skapar vi en ```credentials``` variabel som innehåller användarnamn och lösenord för den grundläggande autentiseringen.

```
defmodule Example do
  use HTTPoison
  
  @base_url "https://example.com/api"
  credentials = {:basic_auth, "username", "password"}
  
  # Skicka en GET-förfrågan
  response = get(@base_url, credentials)
  
  # Skicka en POST-förfrågan med data
  body = %{name: "John", age: 30}
  response = post(@base_url, body, credentials)
end
```

När vi skickar en GET-förfrågan, behöver vi bara ange bas URL och grundläggande autentiseringsuppgifterna. Men när vi skickar en POST-förfrågan, måste vi också ange data som vi vill skicka.

## Djupdykning
För att förstå hur HTTP-förfrågan med grundläggande autentisering fungerar bakom kulisserna, måste vi titta på hur ```HTTPoison``` modulen arbetar. När vi använder ```get``` eller ```post``` funktionerna, gör modulen faktiskt ett anrop till funktionen ```request``` som tar emot olika parametrar såsom URL, metod, data och autentiseringsuppgifter.

För grundläggande autentisering, skickar ```HTTPoison``` en ```Authorization``` header med värdet av användarnamn och lösenord som är kodade i base64. Detta säkerställer att våra autentiseringsuppgifter är krypterade och säkra vid överföring.

Vid mottagandet av förfrågan, kommer servern att verifiera autentiseringsuppgifterna genom att avkoda base64-värdet och matcha det med användarnamnet och lösenordet som lagrats på servern. Om det är en matchning, kommer servern att skicka tillbaka en 200 OK-status och vi kommer att få ett svar från vår förfrågan.

## Se också
- [HTTPoison dokumentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Elixir officiell webbplats](https://elixir-lang.org/)
- [HTTP Basic Authentication - Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)