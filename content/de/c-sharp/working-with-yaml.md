---
title:                "Arbeiten mit YAML"
html_title:           "C#: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit YAML beschäftigen? Nun, YAML ist eine äußerst nützliche und beliebte Möglichkeit, Daten zu strukturieren und zu speichern. Es ist platzsparend, lesbar und leicht zu verstehen, was es zu einer großartigen Wahl für Anwendungen macht, die komplexe Datenstrukturen benötigen.

## Wie gehts

Um mit YAML in C# zu arbeiten, müssen wir zuerst eine YAML-Bibliothek installieren. Eine beliebte Option ist die "YamlDotNet" Bibliothek, die wir über die NuGet-Paket-Manager-Konsole installieren können:

```
Install-Package YamlDotNet
```

Anschließend müssen wir das `YamlStream` Objekt importieren, um unsere YAML-Datei zu lesen:

```
using YamlDotNet.Core;
```

Als nächstes können wir unsere YAML-Datei mithilfe des `YamlStream` Objekts öffnen und durchlaufen. Zum Beispiel, um eine Liste von Personen mit ihren Namen und Alter auszugeben, können wir folgenden Code verwenden:

```
YamlStream stream = new YamlStream();
stream.Load(new StreamReader("personen.yml"));

var liste = (YamlSequenceNode)stream.Documents[0].RootNode;
foreach (var person in liste.Children)
{
    var name = person["Name"].ToString();
    var alter = person["Alter"].ToString();
    Console.WriteLine($"Name: {name}, Alter: {alter}");
}
```

Die Ausgabe würde dann folgendermaßen aussehen:

```
Name: Max Mustermann, Alter: 30
Name: Maria Schmitt, Alter: 25
Name: Felix Bauer, Alter: 40
```

## Tiefentauchen

Nun, da wir gesehen haben, wie wir YAML in unserem C# Code lesen können, wollen wir uns nun mit einigen fortgeschrittenen Features auseinandersetzen. Zum Beispiel, wie man benutzerdefinierte Objekte in YAML speichert und lädt.

Um ein Objekt in YAML zu speichern, müssen wir zuerst eine Klasse mit den gewünschten Eigenschaften erstellen. Nehmen wir an, wir haben eine `Person` Klasse mit den Eigenschaften `Name` und `Alter`:

```
public class Person
{
    public string Name {get; set;}
    public int Alter {get; set;}
}
```

Als nächstes müssen wir eine Instanz dieser Klasse erstellen und diese mit Daten befüllen:

```
Person max = new Person()
{
    Name = "Max Mustermann",
    Alter = 30
};
```

Nun können wir unsere Instanz in YAML umwandeln und in einer Datei speichern:

```
var yaml = new YamlStream();
yaml.Save(new StreamWriter("person.yml"));
yaml.Serialize(new MappingNode()
{
    {"Name", max.Name},
    {"Alter", max.Alter}
});
```

Um die Daten später wiederherzustellen, können wir einfach die Datei einlesen und sie der `Person` Klasse zuweisen:

```
var serialized = yaml.Documents[0].RootNode;
Person max = new Person()
{
    Name = serialized["Name"].ToString(),
    Alter = (int)serialized["Alter"]
};
```

Das war nur ein kleiner Einblick in die Funktionalitäten von YAML in C#. Es gibt noch viele weitere Möglichkeiten und Methoden, um mit YAML zu arbeiten. Ich empfehle Ihnen, die offizielle Dokumentation der YamlDotNet Bibliothek zu studieren und eigene Experimente durchzuführen, um mehr über YAML zu erfahren.

## Siehe auch

- Offizielle YamlDotNet Dokumentation: https://github.com/aaubry/YamlDotNet
- Offizielle YAML Spezifikation: https://yaml.org/
- YAML Tutorial für Anfänger : https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/