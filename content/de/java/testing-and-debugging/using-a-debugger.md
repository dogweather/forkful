---
date: 2024-01-26 03:49:30.157433-07:00
description: "Angenommen, Sie haben ein einfaches Java-Programm, das Probleme macht,\
  \ und Sie k\xF6nnen nicht herausfinden, warum. So w\xFCrden Sie einen Debugger mit\
  \ Eclipse\u2026"
lastmod: '2024-03-13T22:44:53.767349-06:00'
model: gpt-4-0125-preview
summary: "Angenommen, Sie haben ein einfaches Java-Programm, das Probleme macht, und\
  \ Sie k\xF6nnen nicht herausfinden, warum. So w\xFCrden Sie einen Debugger mit Eclipse\u2026"
title: Einsatz eines Debuggers
weight: 35
---

## Wie geht das:
Angenommen, Sie haben ein einfaches Java-Programm, das Probleme macht, und Sie können nicht herausfinden, warum. So würden Sie einen Debugger mit Eclipse starten, einer der beliebten IDEs für die Java-Entwicklung:

Zuerst stellen Sie sicher, dass Sie einen Haltepunkt gesetzt haben. Klicken Sie dann mit der rechten Maustaste auf die Datei, wählen Sie 'Debuggen als' und klicken Sie auf 'Java-Anwendung'.

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // Setzen Sie hier einen Haltepunkt
        int result = divide(a, b);
        System.out.println("Das Ergebnis ist: " + result);
    }

    private static int divide(int numerator, int denominator) {
        // Ein weiterer guter Ort für einen Haltepunkt
        return numerator / denominator;
    }
}
```

Dadurch wird Ihr Programm am Haltepunkt pausiert, und Sie können Variablen inspizieren, den Code Zeile für Zeile durchgehen und beobachten, wie sich Ihr Programm verhält.

Beispielausgabe (in einer Debugger-Konsole):
```
Haltepunkt erreicht bei Zeile: int result = divide(a, b);
```

## Vertiefung
Das Konzept des Debuggings gibt es schon seit den frühen Tagen der Programmierung. Der Legende nach stammt der Begriff "Bug" (Englisch für Fehler) tatsächlich von einem echten mottenähnlichen Käfer, der von Grace Hopper, einer Pionierin auf dem Gebiet, in einem Computer gefunden wurde. Bis heute haben wir ausgefeilte IDEs wie IntelliJ IDEA, Eclipse und NetBeans, die leistungsstarke Debugger integrieren.

Alternativen zu IDE-Debuggern umfassen Logging, Print-Befehle (Arme-Leute-Debugger), Assertions und eigenständige Debugging-Tools wie jdb (Java Debugger), der Teil des Java Development Kit (JDK) ist.

Ein Debugger funktioniert, indem er dem Programmierer ermöglicht, die Ausführung zu pausieren (Haltepunkte), den Code Schritt für Schritt durchzugehen, Variablenwerte zu inspizieren, diese Werte spontan zu ändern und sogar Code blockweise auszuführen. Die Verwendung eines Debuggers wird oft als unschätzbare Technik für die Entwicklung komplexer Anwendungen angesehen, bei denen das Auffinden der genauen Codezeile, die ein Problem verursacht, mit der Suche nach einer Nadel im Heuhaufen verglichen werden kann.

## Siehe auch
- Die offizielle Oracle-Dokumentation zum Debuggen: [Oracle Java SE Debugging](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- Der Leitfaden von Eclipse zum Debuggen: [Eclipse Debugging Tipps](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article4.php)
- VisualVM, ein visuelles Werkzeug, das mehrere Befehlszeilen-JDK-Tools und leichte Profilierungsfähigkeiten integriert: [VisualVM](https://visualvm.github.io/)
