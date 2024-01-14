---
title:                "Arduino: Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Warum: Es gibt viele Gründe, warum man prüfen möchte, ob ein Verzeichnis existiert. Zum Beispiel könnte man diese Überprüfung nutzen, um sicherzustellen, dass eine Datei erfolgreich gespeichert wurde oder um sicherzustellen, dass ein bestimmtes Verzeichnis existiert, bevor man versucht, Daten dorthin zu speichern.

Wie man prüft, ob ein Verzeichnis existiert: Um zu überprüfen, ob ein Verzeichnis existiert, kann man die Funktion "exists()" verwenden, die in der Arduino-IDE verfügbar ist. Diese Funktion akzeptiert eine Zeichenfolge, die den Pfad des zu prüfenden Verzeichnisses enthält, und gibt true zurück, wenn das Verzeichnis existiert, oder false, wenn es nicht existiert. Im Folgenden ist ein Beispielcode zu sehen:

```Arduino
if(SD.exists("/Mein-Verzeichnis")){
  Serial.println("Das Verzeichnis existiert!");
}
else{
  Serial.println("Das Verzeichnis existiert nicht!");
}
```

Ausgabe:

```Arduino
Das Verzeichnis existiert!
```

Tiefergehende Informationen: Bevor man versucht, auf ein bestimmtes Verzeichnis zuzugreifen oder Daten dorthin zu speichern, ist es wichtig, zu überprüfen, ob das Verzeichnis überhaupt existiert. Dies kann verhindern, dass Fehler auftreten und das Programm möglicherweise abstürzt. Es ist auch hilfreich, die Funktion "exists()" zu verwenden, um zu prüfen, ob ein neu erstelltes Verzeichnis erfolgreich erstellt wurde. Wenn das Programm weiß, dass das Verzeichnis existiert, kann es sicher darauf zugreifen und Daten speichern. 

Siehe auch: 

- [Arduino Referenz für die Funktion "exists()"](https://www.arduino.cc/reference/en/libraries/sd/existence-functions/exists/) 

- [Tutorial zum Speichern von Daten auf einer SD-Karte mit Arduino](https://create.arduino.cc/projecthub/Marcel/datalogger-getting-data-from-sensors-into-excel-ac