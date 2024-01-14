---
title:    "Gleam: Lesen von Befehlszeilenargumenten"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Einlesen von Befehlszeilenargumenten ist ein wichtiger Bestandteil der Programmierung in Gleam. Es erlaubt dir, deine Programme auf dynamische Weise zu steuern und zu konfigurieren und somit die Flexibilität und Anpassungsfähigkeit deiner Anwendungen zu erhöhen. In diesem Blogbeitrag werden wir uns genauer anschauen, wie du auf einfache Weise Befehlszeilenargumente in deine Gleam-Programme einbindest.

## Wie geht man vor

Um Befehlszeilenargumente in deinen Gleam-Code einzulesen, kannst du die Standardbibliotheksfunktion `gleam/os` verwenden. Hier ist ein Beispielcode, der die Argumente ausgibt:

```Gleam
import gleam/os
import gleam/string.{concat, to_int}

pub fn main() {
  let arguments = os.args()
  let name = arguments[0]
  let age = arguments[1] |> to_int()
 
  let message = concat(["Hello ", name, ", you are ", toString(age), " years old!"])
  
  os.stdout.write_line(message)
}
```

Wenn wir dieses Programm mit den Argumenten "Max 26" ausführen, wird die Ausgabe "Hello Max, you are 26 years old!" sein.

Die `os.args()`-Funktion gibt eine Liste von Strings zurück, wobei der erste String immer der Name des ausgeführten Programms ist. Die restlichen Strings enthalten die eingegebenen Argumente. In unserem Beispiel haben wir zuerst den Namen in einer Variable gespeichert und mit dem `to_int()` Helper-Modul den zweiten String, das Alter, in eine Zahl umgewandelt.

Du kannst die `os.args()`-Funktion auch mit Pattern Matching verwenden, um gezielt auf bestimmte Argumente zuzugreifen. Hier ist ein Beispielcode, der die Ausgabe je nach gegebenem Argument anpasst:

```Gleam
import gleam/os
import gleam/string.{concat, to_int}

pub fn main() {
  let arguments = os.args()
  
  let message =
    case arguments {
      [_, "male"] -> "Hello sir!"
      [_, "female"] -> "Hello madam!"
      [name, age] -> {
        let parsed_age = to_int(age)
        concat(["Hello ", name, "! You are ", toString(parsed_age), " years old."])
      }
      _ -> "Hello there stranger!"
    }
    
  os.stdout.write_line(message)
}
```

## Tiefergehende Informationen

Bei der Verwendung von Befehlszeilenargumenten gibt es einige Dinge zu beachten. Zum Beispiel können Argumente mit Leerzeichen in doppelten Anführungszeichen eingeschlossen werden, damit sie als ein einzelnes Argument interpretiert werden. Du kannst auch Flags verwenden, indem du ein einzelnes oder doppeltes Minuszeichen vor dem Argument setzt (z.B. `-v` oder `--verbose`). Diese Art von Argumenten können mit Pattern Matching erfasst werden, indem du sie als Strings miteinander vergleichst.

Es ist auch wichtig zu beachten, dass die eingelesenen Argumente immer als Strings zurückgegeben werden. Wenn du sie als Zahlen oder andere Datentypen verwenden möchtest, musst du sie entsprechend konvertieren, wie wir es im obigen Beispiel mit dem `to_int()`-Modul gemacht haben.

## Siehe auch

- Die offizielle Dokumentation zu `gleam/os`: https://gleam.run/modules/gleam_os.html
- Ein Tutorials zur Verwendung von Befehlszeilenargumenten in Gleam: https://gist.github.com/mhogar/bc112912362886d547f955756897e608