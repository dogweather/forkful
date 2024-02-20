---
date: 2024-01-20 18:01:30.586517-07:00
description: "HTTP-pyynt\xF6 perusautentikaatiolla on web-palvelimelle l\xE4hetetty\
  \ pyynt\xF6, jossa on k\xE4ytt\xE4j\xE4tunnus ja salasana. Koodarit k\xE4ytt\xE4\
  v\xE4t t\xE4t\xE4 yksinkertaiseen\u2026"
lastmod: 2024-02-19 22:05:15.167520
model: gpt-4-1106-preview
summary: "HTTP-pyynt\xF6 perusautentikaatiolla on web-palvelimelle l\xE4hetetty pyynt\xF6\
  , jossa on k\xE4ytt\xE4j\xE4tunnus ja salasana. Koodarit k\xE4ytt\xE4v\xE4t t\xE4\
  t\xE4 yksinkertaiseen\u2026"
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
---

{{< edit_this_page >}}

## What & Why?
HTTP-pyyntö perusautentikaatiolla on web-palvelimelle lähetetty pyyntö, jossa on käyttäjätunnus ja salasana. Koodarit käyttävät tätä yksinkertaiseen pääsynvalvontaan, kun haluavat suojata resurssejaan.

## How to:
Lähettääksesi HTTP-pyynnön Elixirissä perusautentikaatiolla, voit käyttää `HTTPoison`-kirjastoa. Tässä on pikainen esimerkki:

```elixir
# Add HTTPoison to your mix.exs dependencies if you haven't already:
# {:httpoison, "~> 1.8"}

defmodule BasicAuthExample do
  def send_request do
    auth = encode_credentials("user", "pass")
    HTTPoison.get("http://example.com", [], basic_auth: auth)
  end

  defp encode_credentials(username, password) do
    Base.encode64("#{username}:#{password}")
  end
end

# Call the function and get the response
{:ok, response} = BasicAuthExample.send_request()
```

Tämän pitäisi antaa sinulle vastaus, kuten:

```elixir
{:ok,
 %HTTPoison.Response{
   body: "Response body here...",
   status_code: 200,
   ...
 }}
```

## Deep Dive
Perusautentikaatio on HTTP-protokollan vanhin autentikointitapa. Se on suoraviivainen, mutta ei kauhean turvallinen, koska käyttäjätunnus ja salasana lähetetään koodattuna, mutta ei salattuna. Siksi käytä sitä vain HTTPS-protokollan yli.

Elixirissä `HTTPoison` on suosittu HTTP-asiakaskirjasto. Se tarjoaa yksinkertaisen tavan tehdä pyyntöjä ja käsittää vastauksia. Vaihtoehtona on `Tesla`, joka on joustavampi ja modulaarisempi asiakaskirjasto.

Implementaation yksityiskohdista, lähetät `Authorization`-otsakkeen, joka sisältää `'Basic '`, ja perään Base64-koodatun merkkijonon `'käyttäjätunnus:salasana'`. Serveri dekoodaa ja tarkistaa tämän ennen resurssin paljastamista. 

## See Also
- [HTTPoison GitHub](https://github.com/edgurgel/httpoison)
- [Tesla GitHub](https://github.com/teamon/tesla)
- [Elixir Base module](https://hexdocs.pm/elixir/Base.html)
- [Basic authentication on MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
