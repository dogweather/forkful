---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Nedlasting av en webside innebærer å hente HTML-koden som webserveren sender når du ber om en side. Dette gjør programmerere for å analysere sidens data, lagre den for offline bruk, eller føre overvåkning på siden.

## Hvordan gjør man det:

I Fish Shell kan nedlasting av nettsider oppnås ved hjelp av `curl` eller `wget` kommandoene. Her er et eksempel som viser bruk av begge.

```Fish Shell
# Bruke curl
curl -O http://eksempelside.no

# Bruke wget
wget http://eksempelside.no
```

Hvis alt gikk bra, vil output være noe som dette:

```Fish Shell
# Curl output
  % Total     % Received  % Xferd    Average Speed   Time    Time     Time  Current
                                Dload   Upload  Total   Spent    Left   Speed
100  178k  100  178k  0     0   197k      0 --:--:-- --:--:-- --:--:--  197k

# Wget output
--2022-01-01 12:00:00--  http://eksempelside.no/
Resolving eksempelside.no (eksempelside.no)... 93.184.216.34, 2606:2800:220:1:248:1893:25c8:1946
Connecting to eksempelside.no (eksempelside.no)|93.184.216.34|:80... connected.
HTTP request sent, awaiting response... 200 OK
Length: unspecified [text/html]
Saving to: ‘index.html’
```

## Dypdykk

Opp gjennom historien har vi utviklet mange måter å laste ned websider på. Selv før `curl` og `wget` ble standard kommandoer, brukte programmerere sockets og HTTP-protokoller til å gjøre dette manuelt.

Selv om `curl` og `wget` begge brukes til å laste ned innhold, har de noen forskjeller. `curl` støtter flere protokoller, mens `wget` kan laste ned hele nettsider rekursivt.

Disse kommandoene sender en HTTP GET forespørsel til serveren og lagrer responsen – vanligvis en HTML-fil – i en lokal fil.

## Se også 

1. [Fish Shell Offisiell Dokumentasjon](https://fishshell.com/docs/current/index.html)
2. [cURL Offisiell Dokumentasjon](https://curl.haxx.se/docs/manpage.html)
3. [Wget Offisiell Dokumentasjon](https://www.gnu.org/software/wget/manual/wget.html) 
4. [HTTP GET forespørsel](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/GET) på MDN Web Docs.