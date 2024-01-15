---
title:                "Herunterladen einer Webseite"
html_title:           "Fish Shell: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Stell dir vor, du möchtest eine Webseite herunterladen, um sie offline zu lesen oder als Teil eines größeren Skripts zu verwenden. Mit der Fish Shell kannst du dies schnell und einfach erledigen.

## Wie geht das?

```Fish Shell
wget <URL>
```

Der Befehl `wget` steht für "Web Get" und kann verwendet werden, um eine beliebige Datei von einer URL herunterzuladen. Wenn du jedoch nur die Webseite herunterladen möchtest, ohne Bilder oder andere Dateien, kannst du die Option `-p` hinzufügen.

```Fish Shell
wget -p <URL>
```

Mit `wget` kannst du auch Dateien von FTP- oder HTTPS-Servern herunterladen und sogar Bandbreiteneinschränkungen festlegen.

## Tief eintauchen

Wenn du mehr Kontrolle über den Downloadprozess haben möchtest, kannst du auch das Programm `curl` verwenden. Im Gegensatz zu `wget` erfordert `curl` jedoch etwas mehr Codierung.

```Fish Shell
curl <URL> -o <Dateiname>
```

Mit `curl` kannst du auch den Download in den Hintergrund verschieben, um andere Aufgaben auszuführen, und du kannst die Serverauthentifizierung konfigurieren, falls erforderlich.

## Siehe auch 

- Offizielle Fish Shell Dokumentation über `wget`: https://fishshell.com/docs/current/cmds/wget.html
- Offizielle Fish Shell Dokumentation über `curl`: https://fishshell.com/docs/current/cmds/curl.html
- Einführung in die Fish Shell: https://fishshell.com/docs/current/tutorial.html