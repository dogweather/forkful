---
title:    "Gleam: Erstellen einer temporären Datei"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Das Erstellen einer temporären Datei ist ein nützliches Werkzeug für Gleam-Entwickler. Es ermöglicht das vorübergehende Speichern von Daten während der Ausführung eines Programms und hilft bei der Vermeidung von Konflikten mit bereits bestehenden Dateien.

## Wie man eine temporäre Datei erstellt

Das Erstellen einer temporären Datei in Gleam ist einfach und erfordert nur wenige Zeilen Code. Verwenden Sie die Standardbibliotheksfunktion `File.Temporary.create/0`, um eine temporäre Datei zu erstellen. Schauen wir uns ein Beispiel an:

```Gleam
temp_file = File.Temporary.create()

# Ausgabe:
Temporäre Datei erstellt: /tmp/glgK7z8nQq

```

Wie Sie sehen können, gibt die Funktion `File.Temporary.create/0` einen `TempFile` zurück, der den Pfad und den Dateinamen der erstellten temporären Datei enthält.

Sie können auch optional einen benutzerdefinierten Namenspräfix angeben, der vor dem Dateinamen platziert wird:

```Gleam
temp_file = File.Temporary.create(prefix: "gleam")

# Ausgabe:
Temporäre Datei erstellt: /tmp/gleamK7z8nQq
```

Die temporäre Datei wird automatisch gelöscht, wenn das Programm beendet wird oder wenn der `TempFile` Wert zerstört wird. Sie müssen sich also keine Sorgen über das Aufräumen der temporären Datei machen.

## Tiefer Einblick

Um genau zu verstehen, wie das Erstellen einer temporären Datei in Gleam funktioniert, werfen wir einen Blick auf die zugrunde liegende Implementierung. Die `File.Temporary.create/0` Funktion verwendet die Standardbibliotheksfunktion `File.new/1`, um eine neue Datei zu erstellen. Dann wird die Funktion `System.mkstemp/0` aus dem Modul `System.Temporary` verwendet, um den Dateinamen und den Pfad der temporären Datei zu erstellen.

Bei der Verwendung der `TempFile` Struktur empfehle ich, das `pattern_matching` zu verwenden, um auf den Dateinamen und den Pfad zuzugreifen:

```Gleam
temp_file = File.Temporary.create()

# Mustermatching
case temp_file {
   TempFile(path, name) -> IO.print("Temporäre Datei erstellt: $(path)/$(name)")
}
```

Dies ist vorteilhaft, da es die Berechnung der Werte `path` und `name` auf einen einzigen Aufruf von `File.Temporary.create/0` reduziert.

## Siehe auch

- `File.Temporary.create/0` offizielle Dokumentation: https://gleam.run/documentation/stdlib/file/#temporary-create-0
- `System.mkstemp/0` offizielle Dokumentation: https://gleam.run/documentation/stdlib/system/temporary/#mkstemp-0
- Offizielle Gleam-Website: https://gleam.run/de/