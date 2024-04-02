---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:24.090170-07:00
description: "Die Integration von PowerShell mit JSON (JavaScript Object Notation)\
  \ bezieht sich auf das Parsen (Lesen) und Generieren (Schreiben) von JSON-Daten,\
  \ einem\u2026"
lastmod: '2024-03-13T22:44:54.124847-06:00'
model: gpt-4-0125-preview
summary: "Die Integration von PowerShell mit JSON (JavaScript Object Notation) bezieht\
  \ sich auf das Parsen (Lesen) und Generieren (Schreiben) von JSON-Daten, einem\u2026"
title: Arbeiten mit JSON
weight: 38
---

## Was & Warum?

Die Integration von PowerShell mit JSON (JavaScript Object Notation) bezieht sich auf das Parsen (Lesen) und Generieren (Schreiben) von JSON-Daten, einem gängigen Format für den Datenaustausch im Web. Programmierer arbeiten mit JSON, um mit Web-APIs zu interagieren, Konfigurationsdateien zu nutzen oder den Datenaustausch zwischen verschiedenen Sprachen und Plattformen aufgrund seiner leichten und sprachunabhängigen Natur zu erleichtern.

## Wie geht das:

### JSON parsen

Um JSON in PowerShell zu lesen oder zu parsen, können Sie das Cmdlet `ConvertFrom-Json` verwenden. Angesichts eines JSON-Strings konvertiert dieses Cmdlet ihn in ein PowerShell-Objekt.

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

Beispielausgabe:

```
John Doe
```

Dieses Beispiel zeigt, wie man einen einfachen JSON-String parst, um auf Eigenschaften des resultierenden Objekts zuzugreifen.

### JSON generieren

Um JSON aus einem PowerShell-Objekt zu generieren, können Sie das Cmdlet `ConvertTo-Json` verwenden. Dies ist praktisch, um Daten für die Übermittlung an einen Webdienst oder das Speichern in einer Konfigurationsdatei vorzubereiten.

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

Beispielausgabe:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

Dieser Codeausschnitt erstellt ein PowerShell-Objekt und konvertiert es dann in einen JSON-String.
