---
title:                "C#: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie in der Welt der Softwareentwicklung tätig sind, dann haben Sie sicherlich schon einmal von YAML gehört. Aber was ist YAML genau und warum sollte es für Sie als Entwickler wichtig sein?

YAML, das für "YAML Ain't Markup Language" steht, ist eine Auszeichnungssprache, die verwendet wird, um Datenstrukturen in einem einfach zu lesenden Format darzustellen. Es wird häufig verwendet, um Konfigurationsdateien für Softwareprojekte zu erstellen.

Wenn Sie sich mit der Verwendung von YAML vertraut machen, können Sie Ihre Konfigurationen auf eine leicht verständliche und wartbare Weise darstellen. Egal, ob Sie Anfänger oder erfahrener Entwickler sind, YAML ist eine nützliche Fähigkeit, die Sie beherrschen sollten.

## Anleitung

Um mit YAML in C# zu arbeiten, benötigen Sie zunächst eine Bibliothek, die YAML-Dateien unterstützt. Eine beliebte Option ist die "YamlDotNet" Bibliothek, die Sie über NuGet in Ihr Projekt installieren können.

Sobald Sie die Bibliothek installiert haben, können Sie mit der Verwendung von YAML in Ihrem Code beginnen. Schauen wir uns ein Beispiel an:

```C#
var config = new YamlStream();
using (var reader = new StreamReader("config.yaml"))
{
    config.Load(reader);
}

var settings = (YamlMappingNode)config.Documents[0].RootNode;

var server = settings.Children["server"].ToString();
var port = settings.Children["port"].ToString();

Console.WriteLine($"Server: {server}");
Console.WriteLine($"Port: {port}");
```

In diesem Beispiel laden wir eine YAML-Datei namens "config.yaml" mit den gewünschten Konfigurationseinstellungen. Dann lesen wir die Daten aus der Datei und geben sie auf der Konsole aus.

Die Verwendung von YAML in C# ist sehr ähnlich wie die Arbeit mit anderen Datenstrukturen wie JSON oder XML. Sie können auch komplexe Datenstrukturen mit verschachtelten Arrays und Objekten erstellen. Mit ein wenig Übung werden Sie schnell lernen, wie Sie Ihre Konfigurationsdateien in YAML erstellen und lesen können.

## Tiefergehende Einblicke

Wenn Sie YAML noch besser verstehen wollen, gibt es einige weitere Dinge, die Sie wissen sollten. Zum Beispiel können Sie in YAML Kommentare mit dem Symbol "#" einfügen, um den Code besser zu dokumentieren und zu organisieren.

Zudem gibt es in YAML sogenannte Anker und Verweise, die es ermöglichen, Werte wiederzuverwenden oder zu aktualisieren. Dies kann besonders nützlich sein, wenn Sie mit komplexen Datenstrukturen arbeiten.

Außerdem gibt es noch viele weitere Funktionen und Möglichkeiten, die Sie erkunden können, um das Beste aus YAML in Ihrer C#-Entwicklung herauszuholen. Stöbern Sie gerne in der offiziellen Dokumentation oder suchen Sie nach praktischen Tutorials und Beispielen im Internet.

## Siehe auch

- [YamlDotNet GitHub repository](https://github.com/aaubry/YamlDotNet)
- [Offizielle YAML-Dokumentation](https://yaml.org/)
- [Ein Tutorial zur Verwendung von YAML in C#](https://www.pluralsight.com/guides/c-sharp-working-with-yaml)