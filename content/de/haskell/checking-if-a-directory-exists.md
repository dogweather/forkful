---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Haskell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Verzeichnis Existenzprüfung in Haskell

## Was & Warum?

Die Überprüfung, ob ein Verzeichnis existiert, ist eine Operation, um das Vorhandensein eines bestimmten Pfads im Dateisystem festzustellen. Es ermöglicht Programmierern, Fehlern bei Dateioperationen vorzubeugen.

## So wird's gemacht:

In Haskell könnten wir die `doesDirectoryExist` Funktion aus dem `System.Directory` Modul verwenden, um dies zu erreichen. Hier ist ein Codeausschnitt:

```Haskell
import System.Directory

checkDirectory :: IO ()
checkDirectory = do
    result <- doesDirectoryExist "pfad/zum/verzeichnis"
    if result
    then putStrLn "Das Verzeichnis existiert."
    else putStrLn "Das Verzeichnis existiert nicht."
```

Was passiert hier? Der `doesDirectoryExist`-Aufruf gibt ein `IO Bool` zurück. Dieses Resultat wird überprüft und darauf entsprechend reagiert.

## Tiefere Tauchgänge

Haskell implementiert die Verzeichnisprüfung grundsätzlich über das zugrunde liegende Betriebssystem. Im Hintergrund wird auf POSIX-Systemen ein `stat`-Systemaufruf ausgeführt und die Rückgabe interpretiert.

Andere Methoden sind ein direkter Systemaufruf in C oder der Einsatz von externen Shell-Befehlen, die jedoch in der Regel weniger idiombasiert und potenziell fehleranfälliger sind.

Historisch gesehen war das Arbeiten mit Dateien und Verzeichnissen in Haskell immer ein wenig umständlich, da diese Operationen Nebeneffekte haben und daher in den IO-Monaden behandelt werden müssen. Mit der Einführung des `System.Directory`-Moduls wurde jedoch versucht, dem entgegenzuwirken und eine bequemere API bereitzustellen.

## Siehe auch

Weitere Informationen und Anleitungen zum Umgang mit Dateien und Verzeichnissen in Haskell finden Sie in den folgenden Links:

- [System.Directory in Haskell](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)