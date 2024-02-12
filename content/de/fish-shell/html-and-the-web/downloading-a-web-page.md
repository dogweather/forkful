---
title:                "Webseite herunterladen"
aliases:
- /de/fish-shell/downloading-a-web-page.md
date:                  2024-01-20T17:43:57.845974-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite bedeutet, ihre Daten zu erfassen, um sie offline zu analysieren oder zu verarbeiten. Programmierer machen das, um Inhalte zu sammeln, automatisiert zu arbeiten oder Webanwendungen zu testen.

## How to:
Um eine Webseite herunterzuladen, verwenden wir `curl` oder `wget`. Hier ist ein einfaches Beispiel mit `curl`:

```Fish Shell
curl https://example.com -o meine_webseite.html
```

Das lädt den Inhalt von `example.com` herunter und speichert ihn in `meine_webseite.html`. Mit `wget` ist es fast genauso simpel:

```Fish Shell
wget -O meine_webseite.html https://example.com
```

Die Ausgabe ist die Webseite, die als `meine_webseite.html` auf deinem Computer liegt.

## Deep Dive
Herunterladen von Webseiten ist so alt wie das World Wide Web selbst. Ursprünglich von Hand durchgeführt, automatisieren Tools wie `curl` und `wget` den Prozess jetzt. 

`curl` unterstützt viele Protokolle und ist bekannt für seine Vielfältigkeit. `wget` hingegen, ist ideal für rekursive Downloads oder das Spiegeln von Seiten.

Für größere Projekte kann man zu spezialisierten Libraries wie Python's `requests` oder `BeautifulSoup` greifen, um die Daten nach dem Herunterladen zu manipulieren.

Hinter den Kulissen nutzen `curl` und `wget` HTTP-Requests, um mit dem Webserver zu kommunizieren und den Seiteninhalt abzurufen.

## See Also
- [curl's offizielle Webseite](https://curl.se/)
- [Wget's GNU-Seite](https://www.gnu.org/software/wget/)
- [HTTP-Requests verstehen](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [BeautifulSoup Dokumentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
