---
title:                "Skicka en http-begäran med grundläggande autentisering."
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering."
simple_title:         "Skicka en http-begäran med grundläggande autentisering."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP-förfrågan med grundläggande autentisering kan vara användbart när man behöver säkerställa att en förfrågan kommer från en auktoriserad användare innan åtgärder vidtas. Det är en enkel och grundläggande autentiseringsmetod som är vanligt förekommande inom webbutveckling.

## Så här gör du

För att skicka en HTTP-förfrågan med grundläggande autentisering i Elixir, behöver du först installera biblioteket `HTTPoison` som tillhandahåller funktioner för att bygga och skicka HTTP-förfrågningar.

```
mix deps.get
```

Först importerar du biblioteket i din kod:

```
import HTTPoison
```

För att skicka en förfrågan behöver du ange URL:en till den resurs du vill nå och autentiseringens användarnamn och lösenord i en `username` och `password`-tupel. Du kan också lägga till eventuella parametrar eller en request body i en `params` variabel.

```
url = "https://exempel.com/api/produkter"
username = "användarnamn"
password = "lösenord"
params = %{kund: "John Doe", produkt: "elcykel"}
```

Sedan kan du använda funktionen `get/4` eller `post/4` beroende på vilken förfrågan du vill göra. Funktionen tar också ett valfritt argument med alternativa http-förfrågningsparametrar, t.ex. om du behöver sätta en timeout eller ange önskat format på svaret.

```
get(url, username, password, params, timeout: 5000, as: :json)
```

Du kan sedan hantera svaret från förfrågan genom att använda Elixir's pattern matching för att extrahera den relevanta informationen från responsen.

```
case response do
  {:ok, %{status_code: 200, body: body}} ->
    # gör något med body
  {:ok, %{status_code: 401}} ->
    # felaktig autentisering
  {:error, %{reason: reason}} ->
    # fel vid förfrågan
end
```

## Djupdykning

HTTP-begäranden med grundläggande autentisering skickar användarnamn och lösenord i klartext, vilket kan betraktas som säkerhetshot. Det är därför viktigt att använda HTTPS för att kryptera förfrågan och skydda känslig information.

Även om grundläggande autentisering är en enkel och snabbmetod för autentisering, finns det andra mer avancerade autentiseringsmetoder som kan vara bättre lämpade för vissa användningsfall. Det är viktigt att undersöka vilken autentiseringsmetod som passar bäst för ditt specifika projekt.

## Se även

- Officiell Elixir dokumentation för HTTPoison: https://hexdocs.pm/httpoison/
- En detaljerad guide om Elixir, HTTP och REST: https://adoptingerlang.org/docs/networking-elixir/
- En artikel om att använda autentisering och auktorisationsbibliotek i Elixir: https://www.cogini.com/blog/up-and-running-with-authentication-and-authorization-in-elixir-phoenix/