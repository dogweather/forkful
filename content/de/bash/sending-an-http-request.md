---
title:                "Eine http Anfrage senden"
html_title:           "Bash: Eine http Anfrage senden"
simple_title:         "Eine http Anfrage senden"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Versenden von HTTP-Anfragen ist ein wichtiger Bestandteil der Webentwicklung. Dabei wird eine Anfrage an einen Server geschickt, der dann entsprechend antwortet. Programmierer nutzen dieses Konzept, um Daten von einem Server abzurufen oder zu senden.

Wie geht's?
Im Bash gibt es verschiedene Möglichkeiten, um HTTP-Anfragen zu senden. Eine davon ist die Nutzung des Befehls ```curl```. Mit diesem Befehl kann eine URL angegeben werden, um eine entsprechende Anfrage an den Server zu senden. Zum Beispiel: ```curl www.example.com``` 

Für eine genauere Kontrolle über die Anfrage, können auch Parameter hinzugefügt werden. Zum Beispiel, um eine POST-Anfrage zu senden: ```curl --data "name=John&age=30" www.example.com```

Alternativ kann auch der Befehl ```wget``` benutzt werden, um eine Datei von einem Server herunterzuladen. Zum Beispiel: ```wget www.example.com/file.txt```

Wenn Sie die Antwort des Servers in einer Variable speichern möchten, können Sie den Befehl ```$(curl www.example.com)``` verwenden. Dies ist besonders nützlich, wenn Sie die Antwort weiterverarbeiten möchten.

Tiefer Einblick
Das Konzept des HTTP-Anforderungs-Antwort-Zyklus wurde bereits 1989 von Tim-Berners Lee vorgeschlagen und ist bis heute das Hauptprotokoll für den Austausch von Daten im Web. Neben den genannten Befehlen gibt es auch andere Tools wie ```wget```, ```httpie``` oder ```lynx```, die für die Durchführung von HTTP-Anfragen verwendet werden können.

Wenn Sie noch mehr Kontrolle über die Anfragen haben möchten, können Sie sich auch mit den verschiedenen Optionen des ```curl```-Befehls auseinandersetzen. Hier können Sie zum Beispiel verschiedene Header oder Authentifizierungsarten angeben.

Siehe auch
Weitere Informationen zur Verwendung von HTTP-Anfragen in Bash finden Sie in der offiziellen Dokumentation zu ```curl``` and ```wget```.