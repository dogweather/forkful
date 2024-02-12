---
title:                "Fehlerbehandlung"
aliases:
- de/vba/handling-errors.md
date:                  2024-02-01T21:55:15.211507-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fehlerbehandlung"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/vba/handling-errors.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Fehlerbehandlung in Visual Basic für Anwendungen (VBA) bezieht sich auf den Prozess der Vorwegnahme, Erkennung und Behebung von Programmier-, Anwendungs- oder Kommunikationsfehlern. Eine robuste Fehlerbehandlung zu implementieren, ist entscheidend für die Aufrechterhaltung der Integrität von Anwendungen und die Verbesserung der Benutzererfahrung, indem unerwartete Probleme geschickt verwaltet werden, ohne abrupte Abstürze oder Datenverlust zu verursachen.

## Wie:

In VBA wird die Fehlerbehandlung typischerweise mit der Anweisung `On Error` implementiert, die VBA anweist, wie weiter verfahren wird, wenn ein Fehler auftritt. Die gängigsten Strategien für die Fehlerbehandlung beinhalten das `On Error GoTo`-Label, `On Error Resume Next` und `On Error GoTo 0`.

**Beispiel 1: Verwendung von `On Error GoTo`**

Dieser Ansatz ermöglicht es Ihnen, das Programm zu einem spezifischen Abschnitt des Codes zu leiten, der unmittelbar nach dem Auftreten eines Fehlers markiert wird.

```vb
Sub ErrorHandlerExample()
    On Error GoTo ErrHandler
    Dim intDivision As Integer

    intDivision = 5 / 0 ' Dies wird einen Teilen-durch-Null-Fehler verursachen

    Exit Sub
ErrHandler:
    MsgBox "Ein Fehler ist aufgetreten: " & Err.Description, vbCritical, "Fehler!"
    Resume Next
End Sub
```

In diesem Beispiel wird jeder Laufzeitfehler den Sprung zu `ErrHandler` auslösen, wobei eine Fehlermeldung angezeigt und dann mit der nächsten Zeile nach dem Fehler fortgefahren wird.

**Beispiel 2: Verwendung von `On Error Resume Next`**

Diese Strategie weist VBA an, mit der Ausführung der nächsten Zeile des Codes fortzufahren, auch wenn ein Fehler auftritt, was nützlich sein kann für Fehler, die als harmlos angesehen werden oder wenn Sie planen, den Fehler später in der Ausführung zu behandeln.

```vb
Sub ResumeNextExample()
    On Error Resume Next
    Dim intDivision As Integer
    intDivision = 5 / 0 ' Dies wird das Programm nicht stoppen; Fehler wird ignoriert

    ' Prüfen, ob ein Fehler aufgetreten ist
    If Err.Number <> 0 Then
        MsgBox "Ein Fehler ist aufgetreten: " & Err.Description, vbExclamation, "Behandelter Fehler"
        ' Fehler zurücksetzen
        Err.Clear
    End If
End Sub
```

In diesem Fall bricht das Programm bei einem Fehler nicht ab; es prüft, ob ein Fehler aufgetreten ist, behandelt ihn, falls ja, und löscht dann den Fehler.

## Vertiefung

Historisch gesehen hat sich die Fehlerbehandlung in Programmiersprachen von einfachen Goto-Anweisungen zu ausgefeilteren Mechanismen wie Ausnahmen in Sprachen wie Java und C# entwickelt. Die Fehlerbehandlung in VBA ist zwar nicht so leistungsfähig oder flexibel wie moderne Ausnahmebehandlungen, erfüllt jedoch ihren Zweck im Kontext der Anwendung der Sprache zur Automatisierung von Aufgaben in Microsoft Office-Umgebungen.

Die primäre Einschränkung der Fehlerbehandlung in VBA liegt in ihrem etwas umständlichen und manuellen Ansatz, der eine sorgfältige Platzierung des Fehlerbehandlungscodes und ein klares Verständnis des Ausführungsflusses erfordert. Moderne Programmiersprachen bieten in der Regel elegantere Lösungen, wie Try-Catch-Blöcke, die den Fluss automatisch zum Fehlerbehandlungscode lenken, ohne dass manuelle Prüfungen oder Sprünge in der Codeausführung erforderlich sind.

Trotz dieser Einschränkungen sind die Fehlerbehandlungsmechanismen von VBA für die meisten Automatisierungsaufgaben geeignet und können, wenn richtig eingesetzt, die Wahrscheinlichkeit von nicht behandelten Fehlern, die Probleme für Benutzer verursachen, erheblich verringern. Darüber hinaus kann das Verständnis der Fehlerbehandlung in VBA Einblicke in ältere Programmierparadigmen und die Entwicklung von Fehlerbehandlungsstrategien in der Softwareentwicklung bieten.
