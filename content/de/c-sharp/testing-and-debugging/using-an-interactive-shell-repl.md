---
date: 2024-01-26 04:12:11.714086-07:00
description: "Ein REPL oder Read-Eval-Print Loop erm\xF6glicht es Ihnen, C#-Code zu\
  \ tippen und diesen interaktiv auszuf\xFChren. Programmierer nutzen es f\xFCr schnelle\u2026"
lastmod: '2024-02-25T18:49:50.950011-07:00'
model: gpt-4-0125-preview
summary: "Ein REPL oder Read-Eval-Print Loop erm\xF6glicht es Ihnen, C#-Code zu tippen\
  \ und diesen interaktiv auszuf\xFChren. Programmierer nutzen es f\xFCr schnelle\u2026"
title: Nutzung einer interaktiven Shell (REPL)
---

{{< edit_this_page >}}

## Was & Warum?
Ein REPL oder Read-Eval-Print Loop ermöglicht es Ihnen, C#-Code zu tippen und diesen interaktiv auszuführen. Programmierer nutzen es für schnelle Experimente, zum Debuggen oder um C# zu lernen, ohne den Overhead der Einrichtung vollständiger Projekte.

## Wie geht das:
Starten Sie ein REPL in Ihrer C#-Umgebung, indem Sie das C# Interactive-Fenster verwenden oder `dotnet-script` in Ihrem Terminal ausführen. Hier ist ein Vorgeschmack darauf, wie es verwendet wird:

```csharp
> var greeting = "Hallo, REPL!";
> Console.WriteLine(greeting);
Hallo, REPL!
>
```

Sie erhalten sofortiges Feedback. Keine Schritte zum Kompilieren und Ausführen. Einfach kodieren und sehen.

## Vertiefung
REPL hat seine Reise von Lisp zu modernen Sprachen gemacht und hat in dynamischen Sprachen wie Python großen Erfolg. Mit C# brachte Roslyn das REPL näher an die Entwickler. `csi` für Roslyn und `dotnet-script` für .NET Core sind solide Optionen. Ein tieferer Einblick: Sie bewerten Code zeilenweise, nicht insgesamt, ein anderes Ausführungsmodell im Vergleich zu typischen C#-Anwendungen. Dies beeinflusst die Zustandspersistenz über Ausführungen hinweg und den Geltungsbereich von Variablen.

Das C# Interactive-Fenster von Visual Studio ist ein von Roslyn betriebenes REPL. Es verfügt über Intellisense, mehrere Referenzen und Unterstützung für NuGet-Pakete. Ein großer Schritt gegenüber frühen Befehlszeilenexperimenten.

Für alternative Sprachen verwendet Python `IDLE`, JavaScript hat das REPL von Node.js, und F# wird mit `F# Interactive` geliefert. Jedes fördert sofortige Feedbackschleifen, die für das Testen von kleinen Codestücken oder das Verständnis von Sprachfunktionen unschätzbar sind.

## Siehe auch
- [.NET Core `dotnet-script` REPL](https://github.com/filipw/dotnet-script)
