---
title:                "Ein neues Projekt starten"
aliases:
- /de/go/starting-a-new-project/
date:                  2024-02-03T18:09:22.568066-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ein neues Projekt starten"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/starting-a-new-project.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Ein neues Projekt in Go zu starten, umfasst das Einrichten eines Arbeitsbereichs und dessen Initialisierung mit den notwendigen Go-Modulen. Programmierer tun dies, um Code zu organisieren, Abhängigkeiten effektiv zu verwalten und Build-Prozesse zu erleichtern. Dies ist grundlegend für die Erstellung skalierbarer und wartbarer Software in Go.

## Wie:

Stellen Sie zuerst sicher, dass Go installiert ist, indem Sie `go version` in Ihrem Terminal ausführen. Sie sollten die installierte Version von Go als Ausgabe sehen. Als Nächstes starten wir ein neues Projekt. Navigieren Sie zu Ihrem Arbeitsbereich und führen Sie aus:

```shell
mkdir hello-world
cd hello-world
```

Dies erstellt einen neuen Verzeichnis für Ihr Projekt und versetzt Sie dorthin. Jetzt initialisieren Sie das Modul:

```shell
go mod init example.com/hello-world
```

Ersetzen Sie `example.com/hello-world` durch Ihren Modulpfad. Dieser Befehl erstellt eine `go.mod`-Datei in Ihrem Verzeichnis, die den Start eines neuen Go-Moduls signalisiert. So könnte `go.mod` aussehen:

```plaintext
module example.com/hello-world

go 1.18
```

`go.mod` verfolgt die Abhängigkeiten Ihres Projekts. Jetzt erstellen Sie eine `main.go`-Datei:

```shell
touch main.go
```

Öffnen Sie `main.go` in Ihrem bevorzugten Editor und fügen Sie den folgenden Code hinzu, um "Hallo, Welt!" auszugeben:

```go
package main

import "fmt"

func main() {
    fmt.Println("Hallo, Welt!")
}
```

Um Ihr Programm auszuführen, navigieren Sie zurück zum Terminal und führen Sie aus:

```shell
go run main.go
```

Sie sollten sehen:

```plaintext
Hallo, Welt!
```

Herzlichen Glückwunsch! Sie haben gerade ein neues Go-Projekt gestartet und Ihr erstes Go-Programm ausgeführt.

## Vertiefung

Die Initiative, Module als Standard für die Abhängigkeitsverwaltung in Go einzuführen, war eine bedeutende Veränderung im Go-Ökosystem, die offiziell in Go 1.11 übernommen wurde. Vor Modulen verließen sich Go-Entwickler auf die Umgebungsvariable GOPATH, um Abhängigkeiten zu verwalten, was weniger intuitiv war und oft zur berüchtigten "Abhängigkeitshölle" führte.

Module bieten eine gekapselte Möglichkeit, Projekt-Abhängigkeiten, Versionierung zu verwalten und sind ein Schritt in Richtung der Realisierung von selbstständigeren und portableren Go-Projekten. Jedes Modul gibt seine Abhängigkeiten an, die Go in der Datei `go.mod` nachverfolgt, wodurch die Abhängigkeitsverwaltung in unterschiedlichen Umgebungen und Entwicklungsphasen vereinfacht wird.

Es ist jedoch zu beachten, dass, obwohl Go-Module jetzt der Standard sind, einige ältere Projekte möglicherweise immer noch GOPATH verwenden. Für die meisten neuen Projekte bieten Module ein einfacheres und effektiveres Verwaltungssystem, aber das Verständnis von GOPATH kann nützlich sein, um ältere Go-Codebasen zu warten oder dazu beizutragen.

In Bezug auf Alternativen sind Go-Module nun der de facto Standard, aber die Go-Gemeinschaft hat in der Vergangenheit mit anderen Werkzeugen für die Abhängigkeitsverwaltung wie `dep` experimentiert. Diese wurden jedoch größtenteils durch die offizielle Modulunterstützung ersetzt, die in die Go-Toolchain integriert ist.
