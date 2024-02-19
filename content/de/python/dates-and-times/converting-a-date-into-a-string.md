---
aliases:
- /de/python/converting-a-date-into-a-string/
date: 2024-01-20 17:37:27.605106-07:00
description: "Die Umwandlung eines Datums in einen String bedeutet einfach, ein Datum\
  \ von einem Format, das von einem Computer verstanden wird, in Text umzuwandeln,\
  \ den\u2026"
lastmod: 2024-02-18 23:09:04.474225
model: gpt-4-1106-preview
summary: "Die Umwandlung eines Datums in einen String bedeutet einfach, ein Datum\
  \ von einem Format, das von einem Computer verstanden wird, in Text umzuwandeln,\
  \ den\u2026"
title: Datum in einen String umwandeln
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Die Umwandlung eines Datums in einen String bedeutet einfach, ein Datum von einem Format, das von einem Computer verstanden wird, in Text umzuwandeln, den Menschen leicht lesen können. Programmierer machen das, um Datumsangaben benutzerfreundlich in der UI darzustellen oder um sie in einer Datenbank oder einem Dateinamen zu speichern.

## How to: (Wie geht das:)
```Python
from datetime import datetime

# Aktuelles Datum und Uhrzeit
jetzt = datetime.now()

# Konvertierung in einen String
datum_als_string = jetzt.strftime("%d.%m.%Y %H:%M:%S")

print(datum_als_string)  # z.B. '16.03.2023 14:45:12'
```
Dieser Code gibt dir das aktuelle Datum und die Uhrzeit in einem gut lesbaren Format zurück.

## Deep Dive (Tiefer Tauchgang)
Historisch gesehen verwendet Python die Methode `strftime` (string from time), die ihre Wurzeln in der C-Bibliothek `time.h` hat. Es gibt auch Alternativen wie das `dateutil`-Modul für komplexere Probleme oder das `arrow`-Modul für eine modernere und einfachere API. Die Implementierung der Konvertierung hängt von der Klarheit der gewünschten Ausgabe und der Performance ab. `strftime` bietet eine breite Palette von Direktiven für unterschiedlichste Formate, aber du solltest darauf achten, dass das Ergebnis menschenlesbar und gleichzeitig maschinenkompatibel bleibt.

## See Also (Siehe auch)
- Die Python-Dokumentation zu `datetime`: https://docs.python.org/3/library/datetime.html
- Python's `strftime`-Direktiven: https://strftime.org/
- `dateutil`-Modul: https://dateutil.readthedocs.io/en/stable/
- `arrow`-Modul: https://arrow.readthedocs.io/en/latest/
