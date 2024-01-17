---
title:                "Ein neues Projekt beginnen"
html_title:           "Ruby: Ein neues Projekt beginnen"
simple_title:         "Ein neues Projekt beginnen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was und Warum?
Wenn du als Programmierer ein neues Projekt startest, bedeutet das, dass du eine neue Software oder Anwendung entwickeln möchtest. Das kann aus verschiedenen Gründen passieren, zum Beispiel um eine Geschäftsidee umzusetzen oder um eine bestehende Lösung zu verbessern.

## Wie geht's?
Das Erste, was du tun musst, ist, die nötigen Werkzeuge und Umgebungen für deine Programmiersprache zu installieren. Wenn du mit Ruby arbeitest, kannst du Ruby Version Manager (RVM) nutzen, um verschiedene Ruby-Versionen zu verwalten. Zum Starten eines neuen Projekts musst du dann nur noch einen neuen Ordner erstellen und eine sogenannte Gemfile mit den benötigten Ruby-Gems erstellen. Anschließend kannst du mit dem Schreiben deines Codes beginnen.

```Ruby
# Beispiel Gemfile
source 'https://rubygems.org'

gem 'rspec', '~> 3.0'
gem 'sinatra'
```

## Tiefentauchen
Das Neue-Projekt-Starten ist eine grundlegende Fähigkeit in der Softwareentwicklung, aber es gibt verschiedene Ansätze und Werkzeuge, die dabei helfen können. Eine alternative Methode zum Erstellen von Ruby-Projekten ist die Nutzung des Bundler-Gem. Bundler ist ein Paketmanager, der die Abhängigkeiten deines Projekts automatisch installiert und verwaltet. Somit musst du nicht manuell Ruby-Gems in deine Gemfile einfügen. Außerdem bietet Bundler Funktionen wie das Inkrementieren von Versionen, um eine saubere und gut strukturierte Codebasis zu erhalten.

## Siehe auch
- [Ruby Version Manager](https://rvm.io/)
- [Bundler](https://bundler.io/)
- [Offizielle Ruby-Dokumentation](https://www.ruby-lang.org/de/documentation/)