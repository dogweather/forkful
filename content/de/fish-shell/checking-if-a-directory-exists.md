---
title:                "Fish Shell: Überprüfung der Existenz eines Verzeichnisses"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum
In der Welt der Programmierung ist es oft wichtig zu überprüfen, ob ein bestimmter Ordner existiert oder nicht. Dies kann hilfreich sein, um sicherzustellen, dass ein bestimmter Pfad gefunden wird oder um zu vermeiden, dass Dateien in einem nicht existierenden Ordner gespeichert werden. In diesem Artikel werden wir diskutieren, wie man mit Fish Shell überprüft, ob ein Ordner existiert.

## Wie Man
Um zu überprüfen, ob ein Ordner existiert, können wir den Befehl `test` zusammen mit dem Flag `-d` verwenden. Dies wird uns sagen, ob der angegebene Pfad ein Ordner oder eine Datei ist. Somit können wir Folgendes tun:

```Fish Shell
if test -d /pfad/zum/ordner
    echo "Der Ordner existiert!"
else
    echo "Der Ordner existiert nicht."
end
```

Das Skript wird überprüfen, ob der Ordner `/pfad/zum/ordner` existiert und entsprechend ausgeben. Wir können auch Wildcards verwenden, um eine Menge von Ordnern zu prüfen, z.B.:

```Fish Shell
if test -d /pfad/zum/*/ordner
    echo "Einer oder mehrere der angegebenen Ordner existieren!"
else
    echo "Keiner der angegebenen Ordner existiert."
end
```

Hier wird überprüft, ob mindestens einer der Unterordner von `/pfad/zum` mit dem Namen "ordner" existiert.

## Tiefer Einblick
Der Befehl `test` hat mehrere Flags, die hilfreich sein können, um bestimmte Bedingungen zu überprüfen. Zum Beispiel können wir mit `-e` überprüfen, ob ein beliebiger Pfad existiert, nicht nur ein Ordner.

```Fish Shell
if test -e /pfad/zur/datei
    echo "Die Datei existiert!"
else
    echo "Die Datei existiert nicht."
end
```

Alternativ können wir auch die umgekehrte Bedingung überprüfen, indem wir das Flag `!` verwenden.

```Fish Shell
if test ! -d /pfad/zum/ordner
    echo "Der Ordner existiert nicht!"
else
    echo "Der Ordner existiert."
end
```

Ebenso können wir auch prüfen, ob ein Ordner leer ist, indem wir `-z` verwenden.

```Fish Shell
if test -d /pfad/zum/ordner -a -z /pfad/zum/ordner/*
    echo "Der Ordner ist leer!"
else
    echo "Der Ordner ist nicht leer."
end
```

Hier wird zuerst mit `-a` überprüft, ob der Ordner existiert und dann mit `-z`, ob er leer ist.

## Siehe Auch
- [Fish Shell Dokumentation](https://fishshell.com/docs/current/)
- [Shell Skripting Tutorials auf Deutsch](https://wiki.ubuntuusers.de/Shell/Programmierung/)
- [GeeksforGeeks: Check if a directory exists in Bash](https://www.geeksforgeeks.org/check-if-a-directory-exists-in-bash/)

Das Prüfen, ob ein Ordner existiert, mag auf den ersten Blick wie eine kleine Aufgabe erscheinen, aber es kann sehr nützlich sein, um sicherzustellen, dass unser Skript oder Programm die erwarteten Ergebnisse liefert. Mit den richtigen Techniken und Flags können wir unseren Code noch robuster machen.