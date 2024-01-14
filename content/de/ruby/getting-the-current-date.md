---
title:    "Ruby: Das aktuelle Datum erhalten."
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum

In vielen Programmiersprachen ist es wichtig, das aktuelle Datum und die aktuelle Uhrzeit abzurufen. Besonders in Ruby, da es eine beliebte Sprache für Webentwicklung ist. In diesem Blog-Beitrag werden wir uns das genauer anschauen und sehen, wie wir in Ruby das aktuelle Datum erhalten können.

## Wie man das aktuelle Datum abruft

Die Funktion `Time.now` wird in Ruby verwendet, um das aktuelle Datum und die Uhrzeit abzurufen. Um das aktuelle Datum abzurufen, verwenden wir einfach den Befehl `Time.now` in unserem Code:

```Ruby
puts Time.now
```

Die Ausgabe wird wie folgt aussehen:

`2020-05-23 14:30:00 +0200`

Mit diesem Befehl können wir das aktuelle Datum und die Uhrzeit in diesem Format erhalten. Wenn wir nur das Datum ohne die Uhrzeit erhalten möchten, können wir die Methode `strftime` verwenden und das gewünschte Format angeben:

```Ruby
puts Time.now.strftime("%d/%m/%Y")
```

Die Ausgabe wird dann wie folgt aussehen:

`23/05/2020`

Es gibt verschiedene Formatierungsplatzhalter, die verwendet werden können, um das Datum in verschiedenen Formaten abzurufen. Hier sind einige Beispiele:

- `%d` - Tag des Monats
- `%m` - Monat (als Zahl)
- `%B` - Monatsname
- `%Y` - Vierstelliges Jahr
- `%y` - Zweistelliges Jahr
- `%A` - Wochentag (lang)
- `%a` - Wochentag (kurz)

Um mehr über die Formatierungsplatzhalter zu erfahren, kannst du die [offizielle Dokumentation von Ruby](https://www.ruby-lang.org/en/documentation/faq/5/) besuchen.

## Tieferes Eintauchen

Die `Time`-Klasse in Ruby enthält viele Methoden, mit denen wir das Datum und die Uhrzeit manipulieren können. Hier sind einige Beispiele:

- `Time.now + 60` fügt 60 Sekunden zu der aktuellen Zeit hinzu.
- `Time.now - (60*60)` subtrahiert eine Stunde von der aktuellen Zeit.
- `Time.now.to_i` konvertiert die Zeit in eine Ganzzahl.
- `Time.now.to_f` konvertiert die Zeit in eine Gleitkommazahl.

Es gibt noch viele weitere Methoden und Möglichkeiten, mit Datum und Uhrzeit in Ruby zu arbeiten. Mit ein bisschen Recherche kannst du noch viele weitere nützliche Funktionen entdecken.

## Siehe auch

- [Offizielle Ruby-Dokumentation](https://www.ruby-lang.org/de/documentation/)
- [Ruby für Anfänger](https://www.ruby-lang.org/de/documentation/quickstart/)