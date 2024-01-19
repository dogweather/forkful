---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att du begär data från en server med vissa användaruppgifter. Programmerare gör detta för att få tillgång till skyddade data på en webbserver.

## Hur man gör:
Vi ska använda `curl`, en kraftfull kommandoradsverktyg för att skicka HTTP-begärningar. Nedan är ett exempel på Fish skalskript för att skicka en GET-begäran med grundläggande autentisering.

```fish shell
function send_request
  set url $argv[1]
  set username $argv[2]
  set password $argv[3]

  curl -u "$username:$password" $url
end
```

Anropa funktionen med webbadress, användarnamn och lösenord för att skicka begäran.

```fish shell
send_request http://example.com myUsername myPassword
```

Skriptet ger output från `curl`-kommandot, vilket är serverns svar på din GET-begäran.

## Djupdykning
År 1990 presenterades HTTP-protokollet, följt av Basic Authentication 1991. Dess grundläggande design har inte ändrats mycket sedan dess.

Alternativ till grundläggande autentisering inkluderar Digest Access Authentication, JWT (JSON Web Token) och OAuth. Dock ger grundläggande autentisering en enklare implementation.

För implementationen, `curl -u "$username:$password"` genererar en sträng `$username:$password`, kodar den i Base64 och inkluderar den i HTTP-headers. Servern dekodar headern för att bekräfta användarens autentisering.

## Se även
1. Fish shell dokumentation: [Fish shell official documentation](https://fishshell.com/docs/current/index.html)
2. Mer om curl: [curl official guide](https://curl.haxx.se/docs/guide.html)
3. Mer om HTTP Basic Authentication: [HTTP Basic Authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)