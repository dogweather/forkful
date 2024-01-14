---
title:                "Elixir: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP förfrågan är en grundläggande aspekt av webbutveckling. Det är en viktig del av att interagera med olika webbtjänster och API:er. I denna blogginlägg kommer vi att utforska hur man implementerar detta i Elixir.

## Hur man gör

För att skicka en HTTP förfrågan i Elixir, behöver vi använda oss av inbyggda funktioner som finns tillgängliga i modulen `HTTPoison`. Det finns olika metoder som kan användas för detta, såsom `get`, `post` och `put`. Låt oss se hur vi kan använda dessa med hjälp av kodexempel nedan.

```Elixir
# Använda get-metoden för att hämta data från en URL

response = HTTPoison.get("https://example.com")

IO.inspect response

# Använda post-metoden för att skicka data till en URL

body = %{username: "JaneDoe", password: "123456"}

response = HTTPoison.post("https://api.example.com/login", body)

IO.inspect response

# Använda put-metoden för att uppdatera data på en URL

body = %{name: "JohnDoe", age: 30}

response = HTTPoison.put("https://api.example.com/users/1", body)

IO.inspect response
```

Genom att köra dessa kodexempel i en Elixir-miljö, kommer vi att få utdata som innehåller relevanta information om våra förfrågningar, såsom statuskod, headers och svarskropp.

## Djupdykning

Att skicka en HTTP förfrågan i Elixir är en del av HTTPoisons funktionalitet, men under huven är det faktiskt Erlangs `:httpc` bibliotek som används. Med hjälp av HTTPoison kan vi därför utnyttja alla fördelar som kommer med Erlangs `:httpc` bibliotek, till exempel asynkrona anrop och hantering av fel.

Det är också möjligt att anpassa våra förfrågningar genom att ange specifika headers, timeouts och även autentisering. Detta gör att vi kan använda HTTPoison för mer avancerade användningsområden.

## Se även

- [HTTPoison dokumentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Erlangs :httpc bibliotek](http://erlang.org/doc/man/httpc.html)
- [GenHTTP - en Elixir wrapper för :httpc](https://github.com/LukeWood/gen_http)