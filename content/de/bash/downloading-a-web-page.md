---
title:                "Herunterladen einer Webseite"
html_title:           "Bash: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Herunterladen einer Webseite ist eine häufige Aufgabe für Programmierer, die es ihnen ermöglicht, den Inhalt und die Struktur einer Webseite zu analysieren und zu verwenden. Dies kann nützlich sein, um Daten zu sammeln, automatisierte Aufgaben auszuführen oder einfach nur zu lernen, wie eine bestimmte Webseite aufgebaut ist.

Wie geht das?
Das Herunterladen einer Webseite in Bash ist ganz einfach mit dem Befehl ```curl```. Zum Beispiel, um die Webseite "www.example.com" herunterzuladen, können Sie den Befehl ```curl www.example.com``` verwenden. Dies wird den Inhalt der Webseite auf Ihrem Bildschirm ausgeben.

Für eine etwas fortgeschrittenere Nutzung, können Sie auch den Befehl ```wget``` verwenden. Dies ermöglicht es Ihnen, die heruntergeladene Webseite zu speichern und bestimmte Optionen wie die Download-Geschwindigkeit oder die Anzahl der Versuche anzupassen. Ein Beispielbefehl könnte so aussehen: ```wget -O index.html -c -t 3 www.example.com```. In diesem Befehl wird die heruntergeladene Webseite als "index.html" gespeichert, die Download-Geschwindigkeit wird auf maximal und es werden bis zu drei Versuche unternommen, um die Webseite herunterzuladen.

Tiefere Einblicke
Das Herunterladen von Webseiten in Bash ist ein nützliches Werkzeug, das auf Unix-basierten Systemen seit langem zur Verfügung steht. Es ist schnell, einfach und kann für verschiedene Zwecke eingesetzt werden. Alternativ können Programmierer auch andere Werkzeuge wie Python oder Node.js verwenden, um Webseiten herunterzuladen. Dies kann besonders nützlich sein, wenn Sie zusätzliche Funktionalität wie das Parsen von HTML oder das Speichern von Daten benötigen.

Links
Für weitere Informationen über den Befehl ```curl``` können Sie die offizielle Dokumentation aufrufen unter: https://curl.haxx.se/docs/manpage.html
Weitere Informationen über den Befehl ```wget``` finden Sie hier: https://www.gnu.org/software/wget/
Für eine detailliertere Einführung in das Herunterladen von Webseiten mit Bash, können Sie einen Blick auf diesen Artikel von Linuxize werfen: https://linuxize.com/post/how-to-download-files-using-curl/
Und für Vergleich und Nachschlagezwecke, können Sie diese Dokumentation über das Herunterladen von Webseiten mit Python und Node.js prüfen: https://docs.python.org/3/library/urllib.request.html und https://nodejs.org/api/http.html#http_http_get_options_callback