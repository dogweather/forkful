---
title:                "Verwendung von assoziativen Arrays"
date:                  2024-01-30T19:12:31.197865-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwendung von assoziativen Arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays, auch bekannt als Hashtabellen oder Wörterbücher in PowerShell, ermöglichen es Ihnen, Daten in Schlüssel-Wert-Paaren zu speichern, was das Abrufen von Daten unkompliziert und effizient macht. Programmierer verwenden sie, um verwandte Daten gemeinsam auf eine Weise zu speichern, die einfach über den Schlüssel zugänglich ist.

## Wie:

Das Erstellen und Verwenden von assoziativen Arrays in PowerShell ist ziemlich unkompliziert. Hier ist, wie Sie die Magie bewirken:

**Ein assoziatives Array erstellen:**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "Ingenieur"
```

Dieser Codeausschnitt erstellt ein assoziatives Array mit drei Schlüssel-Wert-Paaren.

**Werte abrufen:**

Um einen Wert zu erhalten, beziehen Sie sich auf seinen Schlüssel:

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**Beispielausgabe:**

```
Alex
```

**Daten hinzufügen oder ändern:**

Verwenden Sie einfach den Schlüssel, um ein neues Paar hinzuzufügen oder ein bestehendes zu ändern:

```PowerShell
$myAssociativeArray["location"] = "New York" # Fügt ein neues Schlüssel-Wert-Paar hinzu
$myAssociativeArray["job"] = "Senior Ingenieur" # Ändert ein bestehendes Paar
```

**Über ein assoziatives Array iterieren:**

Durchlaufen Sie Schlüssel und Werte so:

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**Beispielausgabe:**

```
name : Alex
age : 25
job : Senior Ingenieur
location : New York
```

## Tiefergehend

Das Konzept der assoziativen Arrays ist in vielen Programmiersprachen üblich, in der Regel als Wörterbuch, Karte oder Hashtabelle bezeichnet, abhängig von der Sprache. In PowerShell werden assoziative Arrays als Hashtabellen implementiert, die recht effizient für das Suchen von Schlüsseln, das Speichern von Daten und das Aufrechterhalten einer Sammlung von eindeutigen Schlüsseln sind.

Historisch bieten assoziative Arrays ein Mittel, um Sammlungen von Objekten zu verwalten, bei denen jedes Element schnell ohne Durchlaufen der gesamten Sammlung mit seinem Schlüssel abgerufen werden kann. Die Effizienz des Datenabrufs und der -modifikation in assoziativen Arrays macht sie zu einer bevorzugten Wahl für verschiedene Aufgaben. Sie haben jedoch Einschränkungen, wie z. B. das Aufrechterhalten der Reihenfolge, für die geordnete Wörterbücher oder benutzerdefinierte Objekte eine bessere Alternative sein könnten.

Trotz ihrer Einschränkungen sind assoziative Arrays/Hashtabellen in PowerShell unglaublich flexibel und ein mächtiges Werkzeug für das Scripting. Sie ermöglichen dynamische Datenspeicherung und sind besonders nützlich in Konfigurationen, Datenmanipulation und überall dort, wo ein strukturiertes Datenformat benötigt wird, ohne den Overhead einer formellen Klassendefinition. Denken Sie nur daran, dass assoziative Arrays perfekt für schlüsselbasierte Abrufe sind, aber wenn Ihre Aufgabe komplexe Datenstrukturen erfordert oder eine bestimmte Reihenfolge aufrechterhalten muss, möchten Sie vielleicht andere Datentypen oder benutzerdefinierte Objekte innerhalb von PowerShell erkunden.
