---
title:                "Fish Shell: Lesen von Befehlszeilenargumenten"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie sich in der Welt der Programmierung bewegen, werden Sie früher oder später mit dem Konzept der Befehlszeilenargumente konfrontiert. Diese kleinen Zusätze zu Ihrem Befehl können die Funktionalität und Flexibilität Ihrer Skripte erweitern und helfen Ihnen, Ihre Aufgaben noch schneller zu erledigen. In diesem Artikel erfahren Sie, wie Sie Befehlszeilenargumente mit Fish Shell beherrschen können.

## Wie geht das?

Die Syntax für die Verwendung von Befehlszeilenargumenten in Fish Shell sieht wie folgt aus:

```
fish <skriptname.fish> <argument1> <argument2>
```

Um diese Argumente in Ihrem Skript zu lesen, können Sie die integrierte Funktion `set` verwenden. Hier ein Beispiel:

```
#!/bin/fish

# Befehlszeilenargumente setzen
set argument1 $argv[1]
set argument2 $argv[2]

# Ausgabe
echo "Das erste Argument ist $argument1"
echo "Das zweite Argument ist $argument2"
```

Wenn Sie nun das Skript mit den beiden Argumenten `hallo` und `welt` aufrufen, erhalten wir folgende Ausgabe:

```
Das erste Argument ist hallo
Das zweite Argument ist welt
```

Sie können auch mehrere Argumente auf einmal setzen und auf sie zugreifen. Beachten Sie einfach, dass die Argumente durch Leerzeichen getrennt werden müssen. Zum Beispiel:

```
#!/bin/fish

set argument1 $argv
set argument2 $argv[2]

echo "Alle Argumente: $argument1"
echo "Das zweite Argument: $argument2"
```

Bei Aufruf mit den Argumenten `hallo welt` erhalten wir nun die Ausgabe:

```
Alle Argumente: hallo welt
Das zweite Argument: welt
```

Eine praktische Funktion beim Umgang mit Befehlszeilenargumenten in Fish Shell ist die Verwendung von Flaggen. Diese können verwendet werden, um bestimmte Aktionen auszuführen oder Optionen zu setzen. Hier ein Beispiel:

```
#!/bin/fish

# setzte Standardwerte
set name "Max"
set age 25

# Eingebaute Funktion `contains` überprüfen, ob eine Flagge gesetzt ist
if contains -- $_FLAG_name
    # Argument nach Flagge auslesen
    set name $argv[$_FLAG_name]
endif
if contains -- $_FLAG_age
    set age $argv[$_FLAG_age]
endif

echo "Mein Name ist $name und ich bin $age Jahre alt."
```

Nun können wir das Skript mit den Flaggen `-name` und `-age` sowie den entsprechenden Werten aufrufen, um personalisierte Ausgaben zu erhalten:

```
$ fish <skriptname.fish> -name Tim -age 30
Mein Name ist Tim und ich bin 30 Jahre alt.
```

## Tiefer eintauchen

Sie können in Ihren Skripten auch überprüfen, ob bestimmte Argumente vorhanden sind und entsprechend reagieren. Dazu können Sie die Funktion `contains` verwenden, um zu überprüfen, ob das Argument in der `$argv`-Variablen enthalten ist. Hier ein Beispiel:

```
#!/bin/fish

# setzte Standardwert
set language "en"

# Eingebaute Funktion `contains` überprüfen, ob Argument vorhanden
if contains -- "de" $argv
    set language "de"
endif

# Entscheide, welche Ausgabe gesendet werden soll
switch $language
case "en"
    echo "Hello World!"
case "de"
    echo "Hallo Welt!"
end
```

Ruft man das Skript ohne Argumente auf, wird die englische Version ausgegeben, mit dem Argument `de` wird die deutsche Version ausgegeben.

Sie können auch überprüfen, ob ein Argument gleich einem bestimmten Wert ist. Dazu können Sie `eq` (equal) verwenden. Hier ein Beispiel:

```
#!/bin/fish

# verarbeite optionales Argument
if eq $argv[1] "sicherheit"
    echo "Es geht um Sicherheit!"
else
    echo "Keine besondere Sicherheitswarnung."
end
```

Jetzt können Sie das Skript mit dem optionalen Argument `sicherheit` aufrufen und bekommen eine entsprechende Ausgabe.

## Siehe auch

- [Offizielle Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Befeh