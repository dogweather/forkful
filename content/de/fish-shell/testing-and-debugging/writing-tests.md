---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:28.195044-07:00
description: "Das Schreiben von Tests in Fish Shell beinhaltet das Erstellen von Skripten,\
  \ die automatisch Ihren Code ausf\xFChren, um sein Verhalten gegen\xFCber erwarteten\u2026"
lastmod: '2024-03-13T22:44:54.312993-06:00'
model: gpt-4-0125-preview
summary: "Das Schreiben von Tests in Fish Shell beinhaltet das Erstellen von Skripten,\
  \ die automatisch Ihren Code ausf\xFChren, um sein Verhalten gegen\xFCber erwarteten\
  \ Ergebnissen zu validieren."
title: Tests Schreiben
weight: 36
---

## Was & Warum?

Das Schreiben von Tests in Fish Shell beinhaltet das Erstellen von Skripten, die automatisch Ihren Code ausführen, um sein Verhalten gegenüber erwarteten Ergebnissen zu validieren. Diese Praxis ist entscheidend, da sie sicherstellt, dass Ihre Shell-Skripte wie beabsichtigt funktionieren, wodurch Fehler früh erkannt und die Wartung vereinfacht wird.

## Wie:

Fish hat kein eingebautes Test-Framework wie einige andere Programmierumgebungen. Sie können jedoch einfache Testskripte schreiben, die Behauptungen (Assertions) nutzen, um das Verhalten Ihrer Funktionen zu überprüfen. Zusätzlich können Sie Drittanbieter-Tools wie `fishtape` für eine umfassendere Testumgebung nutzen.

### Beispiel 1: Einfaches Testskript

Beginnen wir mit einer einfachen Funktion in Fish, die die Summe zweier Zahlen berechnet:

```fish
function add --description 'Zwei Zahlen addieren'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

Sie können ein einfaches Testskript für diese Funktion wie folgt schreiben:

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add bestanden"
    else
        echo "test_add fehlgeschlagen"
    end
end

test_add
```

Die Ausführung dieses Skripts würde ausgeben:

```
test_add bestanden
```

### Beispiel 2: Verwendung von Fishtape

Für eine robustere Testlösung können Sie `fishtape`, einen TAP-produzierenden Testrunner für Fish, verwenden.

Installieren Sie zunächst `fishtape`, falls Sie dies noch nicht getan haben:

```fish
fisher install jorgebucaran/fishtape
```

Erstellen Sie als Nächstes eine Testdatei für Ihre `add`-Funktion, z.B. `add_test.fish`:

```fish
test "Addieren von 3 und 4 ergibt 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

Um den Test auszuführen, verwenden Sie den folgenden Befehl:

```fish
fishtape add_test.fish
```

Eine beispielhafte Ausgabe könnte wie folgt aussehen:

```
TAP version 13
# Addieren von 3 und 4 ergibt 7
ok 1 - test_add bestanden
```

Das teilt Ihnen mit, dass der Test erfolgreich war. `fishtape` ermöglicht es Ihnen, detailliertere Tests zu strukturieren und bietet informative Ausgaben, was das Debuggen erleichtert und eine umfassende Testabdeckung für Ihre Fish-Skripte fördert.
