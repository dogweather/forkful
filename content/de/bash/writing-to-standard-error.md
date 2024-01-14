---
title:    "Bash: Schreiben auf den Standardfehler"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

In der Bash-Programmierung gibt es zahlreiche Situationen, in denen es notwendig ist, Fehlermeldungen auszugeben. Dies geschieht in der Regel über den Standardfehlerkanal (standard error) anstelle des Standardausgabe-Kanals (standard output). Aber warum sollte man sich überhaupt die Mühe machen, speziell Fehlermeldungen an den Standardfehlerkanal zu schreiben? Die Antwort ist einfach: um effizienter und präziser zu debuggen. Durch die Ausgabe von Fehlern auf den Standardfehlerkanal können wir Fehlermeldungen von anderen Ausgaben unterscheiden, was uns bei der Fehlersuche und -behebung hilft.

## Wie das funktioniert

Um auf den Standardfehlerkanal zu schreiben, verwenden wir das `>&2`-Syntax in Kombination mit dem `echo`-Befehl. Hier ist ein Beispiel, bei dem wir versuchen, auf eine Datei zuzugreifen, die nicht existiert:

```Bash
datei="nicht_existierende_datei.txt"
if [ ! -f $datei ]; then
    echo "Fehler: Datei $datei existiert nicht" >&2
fi
```

Der `if`-Ausdruck überprüft, ob die Datei nicht vorhanden ist, und wenn dies der Fall ist, wird eine Fehlermeldung an den Standardfehlerkanal geschrieben. Der `echo`-Befehl gibt den angegebenen Text aus, während `>&2` den Standardausgabe-Kanal auf den Standardfehlerkanal umleitet.

Die Ausgabe sieht folgendermaßen aus:

`Fehler: Datei nicht_existierende_datei.txt existiert nicht`

## Tiefere Einblicke

Neben dem `echo`-Befehl gibt es auch andere Möglichkeiten, um auf den Standardfehlerkanal zu schreiben. Zum Beispiel können wir den `printf`-Befehl verwenden, der es uns ermöglicht, Formatierung anzuwenden und Variablen einzufügen. Hier ist ein Beispiel:

```Bash
datei="nicht_existierende_datei.txt"
if [ ! -f $datei ]; then
    printf "Fehler: Datei %s existiert nicht\n" $datei >&2
fi
```

Die Ausgabe wird genauso sein wie beim vorherigen Beispiel. Wir können auch den `source`-Befehl verwenden, um Skripte einzubinden, die speziell dafür geschrieben wurden, Fehlermeldungen zu generieren. Diese Skripte können Funktionen enthalten, die dann einfach in unserem Hauptskript aufgerufen werden können.

## Siehe auch

- [Bash-Kurs für Anfänger](https://wiki.ubuntuusers.de/Bash/Scripting-Tutorial/): Eine ausführliche Anleitung zur Bash-Programmierung.
- [Offizielles Bash-Dokumentationshandbuch](https://www.gnu.org/software/bash/manual/bash.html): Alle Informationen zum Thema Bash auf der offiziellen GNU-Website.
- [Der Standardfehlerkanal in der Shell](https://www.baeldung.com/linux/standard-error-redirect-bash): Ein tieferer Einblick in den Standardfehlerkanal in der Bash-Shell.