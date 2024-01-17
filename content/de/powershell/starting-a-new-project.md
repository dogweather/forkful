---
title:                "Ein neues Projekt beginnen"
html_title:           "PowerShell: Ein neues Projekt beginnen"
simple_title:         "Ein neues Projekt beginnen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?

Wenn wir als Programmierer ein neues Projekt starten, beginnen wir damit eine neue Idee in die Realität umzusetzen. Das kann ein neues Programm, eine Anwendung oder ein Skript sein. Wir starten ein neues Projekt, um unsere Fähigkeiten zu verbessern, kreative Ideen umzusetzen oder auch um einfach nur Spaß an der Programmierung zu haben.

## Wie geht's?

Um ein neues Projekt mit PowerShell zu starten, gibt es mehrere Schritte, die wir befolgen können:

  - Zuerst müssen wir PowerShell auf unserem Computer öffnen.
  - Anschließend erstellen wir einen neuen Ordner für unser Projekt und navigieren zu diesem in PowerShell.
  - Nun können wir ein neues PowerShell-Skript erstellen, indem wir "New-Item -Type File script.ps1" ausführen.
  - Um unser Skript zu bearbeiten, können wir einen Texteditor wie Notepad++ verwenden.
  - Sobald wir mit unserem Code zufrieden sind, können wir das Skript ausführen, indem wir ".\script.ps1" in PowerShell eingeben.

Ein Beispiel für die Erstellung und Ausführung eines einfachen "Hallo Welt!"-Programms in PowerShell:

```PowerShell
New-Item -Type File hello.ps1
notepad hello.ps1
```

In Notepad++:

```PowerShell
Write-Host "Hallo Welt!"
```

In PowerShell:

```PowerShell
.\hello.ps1
```

Output:

```
Hallo Welt!
```

## Tiefere Einblicke

PowerShell wurde von Microsoft entwickelt und ist eine Skriptsprache und Kommandozeilen-Tool für die Windows-Plattform. Es wurde ursprünglich als Ersatz für die veraltete Windows-Befehlszeile eingeführt, bietet aber nun viel mehr Funktionalität und Flexibilität beim Automatisieren von Aufgaben.

Es gibt auch andere Möglichkeiten, ein neues Projekt zu starten, z.B. mit einer integrierten Entwicklungsumgebung (IDE) wie Visual Studio oder mit anderen Programmiersprachen wie Python oder Java. Allerdings bietet PowerShell den Vorteil, dass es bereits auf jedem Windows-Computer vorhanden ist und somit schnell und einfach verwendet werden kann.

Wenn wir tiefer in die Implementierung von Projekten mit PowerShell eintauchen wollen, gibt es viele Ressourcen online, die uns helfen können. Wir können auch die offizielle Dokumentation von Microsoft konsultieren oder uns in Foren und Communities mit anderen PowerShell-Programmierern austauschen.

## Siehe auch

- [Microsoft PowerShell-Dokumentation](https://docs.microsoft.com/en-us/powershell/)
- [PowerShell für Anfänger: Einsteiger-Tutorial](https://www.youtube.com/watch?v=E9jlI_aK08o)
- [PowerShell-Community auf Reddit](https://www.reddit.com/r/PowerShell/)