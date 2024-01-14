---
title:    "Fish Shell: Ein neues Projekt starten"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Als begeisterter Programmierer weißt du wahrscheinlich, wie wichtig es ist, sauberen und effizienten Code zu schreiben. Aber wusstest du, dass die Wahl der richtigen Shell auch einen großen Einfluss auf die Qualität deines Codes haben kann? In diesem Blog-Beitrag werden wir über die Verwendung von Fish Shell sprechen und warum es eine großartige Wahl für dein nächstes Projekt sein könnte.

## Wie geht's 

Fish Shell ist eine benutzerfreundliche, moderne Shell, die für ihre Benutzerfreundlichkeit und Leistung bekannt ist. Sie bietet eine umfassende Autovervollständigungsfunktion, die dir dabei hilft, deine Befehle schneller zu tippen und Fehler zu vermeiden. Lass uns einen kurzen Blick darauf werfen, wie du Fish Shell installieren und verwenden kannst:

```Fish Shell
brew install fish # Installiere Fish Shell mit Homebrew
fish_config # Öffne die Konfigurationsdatei von Fish Shell
```

Nachdem du Fish Shell erfolgreich installiert hast, kannst du mit dem Befehl `fish_config` auf die Fish Shell-Konfiguration zugreifen. Hier kannst du das Design und die Einstellungen der Shell an deine Bedürfnisse anpassen.

## Tiefentauchen

Eine der besten Eigenschaften von Fish Shell ist seine umfassende Autovervollständigungsfunktion. Probieren wir mal aus, wie sie funktioniert:

```Fish Shell
ec# # Drücke die Tab-Taste, um `echo` auszuführen
```

Fish Shell vervollständigt automatisch den Befehl `echo` für dich. Aber das ist nicht alles, Fish Shell vervollständigt auch die Argumente, die du in deinem Befehl verwenden möchtest. Lass uns das mit einem weiteren Beispiel demonstrieren:

```Fish Shell
e#f "Hallo" # Drücke die Tab-Taste, um `echo "Hallo"` auszuführen
```

Wie du siehst, vervollständigt Fish Shell das Argument `"Hallo"` für dich. Dies macht das Schreiben von Befehlen viel einfacher und schneller.

## Siehe auch

- [Fish Shell offizielle Dokumentation] (https://fishshell.com/docs/current/index.html)
- [Fish Shell Github-Repo] (https://github.com/fish-shell/fish-shell)
- [Fish Shell Forum] (https://forum.fishshell.com/)