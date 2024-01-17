---
title:                "Das Versenden einer HTTP-Anfrage"
html_title:           "Fish Shell: Das Versenden einer HTTP-Anfrage"
simple_title:         "Das Versenden einer HTTP-Anfrage"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Was & Warum?
Eine HTTP-Anfrage zu senden bedeutet, dass ein Programm Daten an einen Server schickt und auf eine Antwort wartet. Programmierer tun dies, um Anfragen an APIs oder Webseiten zu stellen und die erhaltenen Daten weiterzuverarbeiten.

# Wie geht's?
Das Senden einer HTTP-Anfrage mit der Fish Shell ist einfach. Dazu verwenden wir den Befehl ```curl```. Erstelle zuerst eine Variable, die die URL enthält, an die du die Anfrage senden möchtest. Dann gib einfach ```curl $url``` ein und voilà, die Antwort des Servers wird direkt in der Shell angezeigt.

```fish
set url "https://example.com"
curl $url
```
Ergebnis:
```
<html>
<head>
<title>Example Domain</title>
...
```

# Tiefer tauchen
Das Versenden von HTTP-Anfragen ist ein Teil des Grundgerüsts des Internets und hat eine lange Geschichte. Es gibt auch andere Methoden, um HTTP-Anfragen zu senden, wie z.B. der Befehl ```wget```. Das Verständnis der verschiedenen Optionen und Parameter von ```curl``` kann auch sehr hilfreich sein, um die Ergebnisse der Anfragen besser zu kontrollieren.

# Siehe auch
- [Offizielle Dokumentation zu curl](https://fishshell.com/docs/current/commands.html#curl)
- [Verwendung von wget zum Senden von HTTP-Anfragen](https://www.computerhope.com/unix/wget.htm)
- [Eine kurze Geschichte des HTTP-Protokolls](https://www.w3.org/Protocols/HTTP/AsImplemented.html)