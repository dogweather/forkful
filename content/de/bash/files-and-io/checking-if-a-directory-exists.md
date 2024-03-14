---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:43.714356-07:00
description: "In der Bash-Programmierung ist die \xDCberpr\xFCfung, ob ein Verzeichnis\
  \ existiert, ein wesentlicher Kontrollmechanismus, der dazu dient, die Existenz\
  \ eines\u2026"
lastmod: '2024-03-13T22:44:54.073821-06:00'
model: gpt-4-0125-preview
summary: "In der Bash-Programmierung ist die \xDCberpr\xFCfung, ob ein Verzeichnis\
  \ existiert, ein wesentlicher Kontrollmechanismus, der dazu dient, die Existenz\
  \ eines\u2026"
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
---

{{< edit_this_page >}}

## Was & Warum?

In der Bash-Programmierung ist die Überprüfung, ob ein Verzeichnis existiert, ein wesentlicher Kontrollmechanismus, der dazu dient, die Existenz eines Verzeichnisses zu bestätigen, bevor Dateioperationen durchgeführt werden. Diese Überprüfung ist entscheidend, um Fehler zu vermeiden, wie zum Beispiel den Versuch, auf Verzeichnisse zuzugreifen oder diese zu ändern, die nicht existieren, und sorgt so für eine reibungslosere und vorhersehbarere Skriptausführung.

## Wie:

Im Kern ermöglicht es Bash, mit bedingten Anweisungen und dem Operator `-d` zu überprüfen, ob ein Verzeichnis existiert. Im Folgenden ist ein einfaches Beispiel, das zeigt, wie diese Überprüfung durchgeführt wird.

```bash
if [ -d "/pfad/zum/verzeichnis" ]; then
    echo "Das Verzeichnis existiert."
else
    echo "Das Verzeichnis existiert nicht."
fi
```

Beispiel Ausgabe (wenn das Verzeichnis existiert):
```
Das Verzeichnis existiert.
```

Beispiel Ausgabe (wenn das Verzeichnis nicht existiert):
```
Das Verzeichnis existiert nicht.
```

Bei komplexeren Skripten ist es üblich, die Überprüfung mit anderen Operationen zu kombinieren, wie zum Beispiel das Erstellen des Verzeichnisses, wenn es nicht existiert:

```bash
VERZ="/pfad/zum/verzeichnis"
if [ -d "$VERZ" ]; then
    echo "$VERZ existiert."
else
    echo "$VERZ existiert nicht. Wird jetzt erstellt..."
    mkdir -p "$VERZ"
    echo "$VERZ erstellt."
fi
```

Beispiel Ausgabe (wenn das Verzeichnis nicht existiert und dann erstellt wird):
```
/pfad/zum/verzeichnis existiert nicht. Wird jetzt erstellt...
/pfad/zum/verzeichnis erstellt.
```

Obwohl Bash selbst robuste Werkzeuge für solche Überprüfungen bereitstellt, gibt es keine beliebten Drittanbieter-Bibliotheken speziell für diese Aufgabe, da native Bash-Befehle vollständig fähig und effizient für die Validierung der Anwesenheit von Verzeichnissen sind.
