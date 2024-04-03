---
date: 2024-01-26 00:52:12.895491-07:00
description: "Fehlerbehandlung erm\xF6glicht es Ihrem Skript, sich anmutig mit dem\
  \ Unerwarteten auseinanderzusetzen. Wir tun dies, um Fehler zu verwalten, ohne das\
  \ Haar\u2026"
lastmod: '2024-03-13T22:44:54.316860-06:00'
model: gpt-4-1106-preview
summary: "Fehlerbehandlung erm\xF6glicht es Ihrem Skript, sich anmutig mit dem Unerwarteten\
  \ auseinanderzusetzen."
title: Fehlerbehandlung
weight: 16
---

## Was & Warum?
Fehlerbehandlung ermöglicht es Ihrem Skript, sich anmutig mit dem Unerwarteten auseinanderzusetzen. Wir tun dies, um Fehler zu verwalten, ohne das Haar unserer Benutzer ergrauen zu lassen.

## Wie geht das:
Um Fehler in Fish zu fangen, nutzen Sie den `status` Befehl und Bedingungen. Angenommen `ping` schlägt fehl; so können Sie das erkennen:

```fish
ping -c 1 example.com
if not status is-success
    echo "Etwas Merkwürdiges ist mit dem Ping passiert."
end
```

Beispielausgabe, wenn `ping` fehlschlägt:

```
Etwas Merkwürdiges ist mit dem Ping passiert.
```

Um einen spezifischen Fehlercode zu behandeln, verwenden Sie `status --is`:

```fish
false
if status --is 1
    echo "Einen Fehler mit Code 1 gefangen."
end
```

Beispielausgabe:
```
Einen Fehler mit Code 1 gefangen.
```

Für einen robusteren Ansatz sollten Sie eine Funktion verwenden:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping fehlgeschlagen mit Status $status"
        return 1
    end
end

try_ping
```

## Tiefer Eintauchen
Die Fehlerbehandlung in Fish entspricht nicht dem `try/catch` Paradigma, das Sie vielleicht von höheren Programmiersprachen kennen. Stattdessen gibt es einfache Exit-Statuscodes, die durch den `status` Befehl bereitgestellt werden.

Historisch gesehen bedeutet in Unix-ähnlichen Systemen ein Exit-Status von `0` Erfolg, während jeder Nicht-Null-Wert einen Fehler anzeigt, der häufig verschiedene Fehlergründe widerspiegelt. Diese Konvention wird von den meisten Kommandozeilen-Utilities verwendet und daher auch von Fish selbst.

Alternativen zu `status` Überprüfungen in Fish beinhalten Signalhandling über `trap` in anderen Shells, aber Fish bevorzugt explizitere Statusüberprüfungen, da sie sauberer sind und weniger anfällig für Nebeneffekte.

In der Umsetzung bleibt die Fehlerbehandlung in Fish einfach und dennoch leistungsfähig, was größtenteils auf ihre nicht-blockierende Natur und die Betonung auf klarer Syntax zurückzuführen ist, wie in den Beispielen gezeigt. Fehlercodes integrieren sich gut mit Funktionen, was eine modulare und lesbare Fehlerverwaltung ermöglicht.

## Siehe Auch
- Fish Dokumentation über Bedingungen: https://fishshell.com/docs/current/language.html#conditionals
- Fish Tutorial zur Fehlerbehandlung: https://fishshell.com/docs/current/tutorial.html#error-handling
