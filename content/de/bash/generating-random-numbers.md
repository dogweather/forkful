---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:48:22.606349-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen in Bash zu generieren bedeutet, numerische Werte zu erzeugen, die nicht vorhersagbar sind. Programmierer nutzen sie für alles Mögliche – von Sicherheitsfunktionen bis hin zu Spielen.

## How to:
```Bash
# Einfache Zufallszahl zwischen 1 und 10
echo $((1 + RANDOM % 10))

# Zufallszahl für einen größeren Bereich, zum Beispiel 1 bis 100
echo $((1 + RANDOM % 100))

# Eine Zufallszahl in einem Skript speichern
my_random_number=$((1 + RANDOM % 100))
echo $my_random_number

# Mit 'shuf' eine Zufallszahl zwischen 1 und 100 generieren:
echo $(shuf -i 1-100 -n 1)
```

Beispiel Ausgabe:
```
7
42
23
85
```

## Vertiefung
Früher wurde `RANDOM` als die Standardmethode angesehen, um in Bash Zufallszahlen zu erhalten. `$RANDOM` ist eine interne Bash-Funktion, die eine Pseudozufallszahl zwischen 0 und 32767 liefert. Alternative Methoden, wie zum Beispiel `shuf` oder das Lesen von `/dev/random`, bieten mehr Flexibilität und in manchen Fällen bessere Zufälligkeit. Bei der Implementierung solltest du beachten, dass `$RANDOM` nicht für kryptografische Zwecke geeignet ist. Dafür sollte man auf spezialisiertere Tools wie `openssl` oder `/dev/urandom` zurückgreifen.

## Siehe Auch
- Bash Manual zum Thema `$RANDOM`: https://www.gnu.org/software/bash/manual/bash.html#index-RANDOM
- `shuf` Kommando: https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html
- Über `/dev/random` und `/dev/urandom`: https://man7.org/linux/man-pages/man4/random.4.html
- OpenSSL und Zufallszahlen: https://www.openssl.org/docs/manmaster/man1/openssl-rand.html