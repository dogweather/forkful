---
title:    "Fish Shell: Eine Datum in eine Zeichenkette umwandeln"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Warum

Wenn Sie mit Fish Shell programmieren, werden Sie merken, dass Sie oft mit Datums- und Zeitangaben arbeiten werden. Eine gängige Aufgabe ist es, ein Datum in einen String umzuwandeln, um es in Ihrem Code anzuzeigen oder weiterzuverarbeiten. In diesem Blog-Beitrag werden wir uns anschauen, wie man dies mithilfe von Fish Shell auf einfache Weise erreichen kann.

## Wie geht das?

Um ein Datum in einen String umzuwandeln, gibt es in Fish Shell die praktische Funktion `strftime`. Diese Funktion nimmt ein Datum als Parameter und gibt den entsprechenden String im gewünschten Format aus. Hier ist ein Beispiel, das das aktuelle Datum in einem benutzerdefinierten Format ausgibt:

```
set today (date)
strftime '%A, %B %e, %Y' $today
```

Dies gibt zum Beispiel den String "Montag, Januar 12, 2021" aus. Sie können das Ausgabeformat nach Ihren eigenen Bedürfnissen anpassen, indem Sie die Parameter in dem `%`-Teil der Funktion `strftime` ändern. Eine vollständige Liste der verfügbaren Parameter finden Sie in der [Dokumentation](https://fishshell.com/docs/3.1/cmds/strftime.html).

## Tief eintauchen

Während die Verwendung von `strftime` eine einfache und schnelle Möglichkeit ist, ein Datum in einen String umzuwandeln, gibt es einige zusätzliche Tipps und Tricks, die Sie beachten sollten. Zum Beispiel können Sie die Funktion `date` verwenden, um ein spezifisches Datum zu erstellen und dann `strftime` darauf anwenden. Sie können auch `now` anstelle von `date` verwenden, um das aktuelle Datum und die Uhrzeit zu erhalten.

Eine weitere hilfreiche Funktion ist `switch`. Diese Funktion kann verwendet werden, um verschiedene Formate basierend auf bestimmten Bedingungen zu wählen. Zum Beispiel können Sie verschiedene Formate für Wochenenden und Wochentage festlegen.

Für weitere Informationen und Beispiele empfehle ich Ihnen, sich mit der [offiziellen Fish-Shell-Dokumentation](https://fishshell.com/docs/3.1/) vertraut zu machen.

## Siehe auch

- [Fish-Shell-Dokumentation](https://fishshell.com/docs/3.1/)
- [Dokumentation zu strftime](https://fishshell.com/docs/3.1/cmds/strftime.html)
- [Online-Forum für Fish Shell](https://github.com/fish-shell/fish-shell/discussions)