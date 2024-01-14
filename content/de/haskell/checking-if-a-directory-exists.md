---
title:                "Haskell: Überprüfen, ob ein Verzeichnis existiert."
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum
Das Überprüfen, ob ein bestimmtes Verzeichnis existiert, ist für viele Programmierer ein wichtiger Schritt in ihren Projekten. Es kann helfen, Fehler zu vermeiden und sicherzustellen, dass der Code reibungslos ausgeführt wird.

## Wie
Eine Möglichkeit, dies in Haskell zu tun, ist die Verwendung der Funktion `doesDirectoryExist` aus dem Modul `System.Directory`. Dies führt eine Überprüfung des angegebenen Pfades durch und gibt `True` zurück, wenn es sich um ein Verzeichnis handelt, und `False`, wenn nicht.

```Haskell
import System.Directory

main :: IO()
main = do
  let directory = "Mein/Verzeichnis"
  exist <- doesDirectoryExist directory
  if exist
    then putStrLn "Das Verzeichnis existiert!"
    else putStrLn "Das Verzeichnis konnte nicht gefunden werden."
```
Dieses Beispiel prüft, ob das Verzeichnis "Mein/Verzeichnis" existiert und gibt entsprechend eine Nachricht aus.

## Deep Dive
Die Funktion `doesDirectoryExist` nutzt unter der Haube die Haskell-Funktion `getPermissions`, um die Berechtigungen für ein bestimmtes Verzeichnis zu überprüfen. Diese Funktion gibt ein `Permissions`-Objekt zurück, das Informationen wie Schreib- und Leserechte enthält. Anhand dieser Informationen entscheidet `doesDirectoryExist`, ob es sich um ein Verzeichnis handelt oder nicht.

Eine wichtige Sache zu beachten ist, dass `doesDirectoryExist` nur überprüft, ob das Verzeichnis existiert und keine tieferen Überprüfungen durchführt. Wenn also ein Fehler auftritt oder das Verzeichnis aus irgendeinem Grund nicht zugänglich ist, wird trotzdem `True` zurückgegeben. Es ist daher wichtig, zusätzliche Überprüfungen durchzuführen, um sicherzustellen, dass das Verzeichnis wirklich existiert.

## Siehe auch
- https://hackage.haskell.org/package/directory-1.3.4.0/docs/System-Directory.html#v:doesDirectoryExist
- https://hackage.haskell.org/package/directory-1.3.4.0/docs/System-Directory.html#v:getPermissions
- https://hackage.haskell.org/package/directory-1.3.4.0/docs/System-Directory.html#t:Permissions