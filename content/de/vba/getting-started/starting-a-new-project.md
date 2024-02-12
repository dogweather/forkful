---
title:                "Einen neuen Projektstart"
aliases: - /de/vba/starting-a-new-project.md
date:                  2024-02-01T22:02:56.686231-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen neuen Projektstart"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/vba/starting-a-new-project.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Ein neues Projekt in Visual Basic for Applications (VBA) zu starten, bedeutet, eine Umgebung innerhalb einer Host-Anwendung wie Excel einzurichten, um Aufgaben zu automatisieren oder die Funktionalität zu erweitern. Programmierer begeben sich auf dieses Gebiet, um die Kraft von VBA zu nutzen, indem sie Microsoft Office-Anwendungen anpassen und automatisieren, wodurch Arbeitsabläufe gestrafft und die Produktivität erhöht wird.

## Wie geht das:

Wenn Sie bereit sind, ein neues VBA-Projekt zu beginnen, besteht der Ausgangspunkt üblicherweise darin, den VBA-Editor zu öffnen und Ihr Projektgerüst zu initialisieren. Lassen Sie uns die Schritte durchgehen, indem wir Excel als Host-Anwendung verwenden:

1. **Öffnen des VBA-Editors**: Drücken Sie in Excel `Alt + F11`, um auf den VBA-Editor zuzugreifen.
2. **Einfügen eines neuen Moduls**: Navigieren Sie im Menü zu `Einfügen > Modul`, um ein neues Modul zu Ihrem Projekt hinzuzufügen. Hier wird Ihr Code liegen.
3. **Erstellen Ihrer ersten Makro**: Lassen Sie uns ein einfaches Makro codieren, das ein Nachrichtenfenster anzeigt. Geben Sie den folgenden Code in das Modul ein:

```vb
Sub SayHello()
    MsgBox "Hallo, Welt!", vbInformation, "Grüße"
End Sub
```

4. **Starten Ihres Makros**: Drücken Sie `F5`, während Ihr Cursor innerhalb der `SayHello`-Prozedur ist, oder gehen Sie zu `Ausführen > Sub/UserForm ausführen` und wählen Sie `SayHello`. Sie sollten ein Nachrichtenfenster mit "Hallo, Welt!" und einem "OK"-Button sehen.

Beispielausgabe:

```plaintext
Ein Nachrichtenfenster mit "Hallo, Welt!" wird angezeigt.
```

5. **Speichern Ihres Projekts**: Bevor Sie beenden, stellen Sie sicher, dass Sie Ihre Arbeit speichern. Wenn Ihr Excel-Arbeitsbuch zuvor nicht gespeichert wurde, werden Sie aufgefordert, es als ein makrofähiges Arbeitsbuch (`.xlsm`-Dateiformat) zu speichern.

## Vertiefung

Visual Basic for Applications ist seit seiner Einführung im Jahr 1993 ein Eckpfeiler in den Automatisierungsstrategien von Microsoft. Als Weiterentwicklung seines Vorgängers, MacroBasic, bot VBA eine robustere Lösung mit verbesserter Integration in die Office-Suite von Microsoft. Der Übergang zu VBA war entscheidend und markierte einen Wechsel zu komplexeren Skriptfähigkeiten, die die Kraft vollwertiger Programmiersprachen nutzten.

Trotz seines Alters bleibt VBA in modernen Büroumgebungen weit verbreitet, vor allem wegen seiner tiefen Integration in Office-Produkte und der umfangreichen Basis an Altcode in vielen Organisationen. Es ist jedoch wichtig zu beachten, dass für neuere, webbasierte Anwendungen oder für Aufgaben, die mehr Skalierbarkeit und Integration mit Nicht-Office-Anwendungen erfordern, Sprachen und Frameworks wie Python, mit seinem reichen Ökosystem an Bibliotheken, oder JavaScript für Office Scripts einen moderneren und vielseitigeren Ansatz bieten. Diese Alternativen, die eine steilere Lernkurve und Einrichtung erfordern, bieten eine breitere Anwendbarkeit und Unterstützung für zeitgemäße Entwicklungspraktiken wie Versionskontrolle und Deployment-Pipelines.
