---
title:                "Skicka en http-förfrågan"
html_title:           "Javascript: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka ett HTTP-begäran är processen att begära data från en server genom HTTP-protokollet. Programmerare gör det för att kommunisera med webbtjänster, hämta data eller göra uppdateringar på distans.

## Hur man gör:
Använd `curl` kommandot för att skicka HTTP-begäran i Bash. Här är ett exempel:

```Bash
curl http://example.com
```

Svar från server:

```Bash
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
</html>
<body>
<div>
    <h1>Example Domain</h1>
</div>
</body>
</html>
```

## Djup Dykning
Historiskt sett, före HTTP-begäran, var datorkommunikation komplicerad och inkonsekvent. HTTP-begäran standardiserade datakommunikation över webben.

Som alternativ till `curl`, kan du även använda `wget`. Skillnaden mellan `curl` och `wget` är att `wget` laddar ner filerna direkt till din disk.

HTTP-begäran är implementerade i Bash genom att använda sockets för att etablera en TCP-koppling till servern. Denna anslutning gör det möjligt för klienten och servern att skicka data fram och tillbaka.

## Se Även
1. [Bash Programming Guide](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
2. [How To Use Curl](https://linuxize.com/post/curl-command-examples/)
3. [Details on HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)