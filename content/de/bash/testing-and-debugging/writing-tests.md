---
title:                "Tests Schreiben"
date:                  2024-02-03T19:29:29.592467-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schreiben"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Testen in Bash bedeutet, Testfälle zu scripten, um die Funktionalität Ihrer Bash-Skripte zu validieren. Programmierer führen Tests durch, um sicherzustellen, dass ihre Skripte unter verschiedenen Bedingungen wie erwartet funktionieren und um Fehler sowie Bugs vor der Bereitstellung zu finden.

## Wie geht das:
Bash verfügt nicht über ein integriertes Testframework, aber Sie können einfache Testfunktionen schreiben. Für anspruchsvolleres Testen sind Drittanbieter-Tools wie `bats-core` beliebt.

### Grundlegendes Testbeispiel in reinem Bash:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "Test bestanden."
    return 0
  else
    echo "Test fehlgeschlagen. Erwartet wurde '$expected_output', erhalten '$result'"
    return 1
  fi
}

# Aufrufen der Testfunktion
test_example_function
```
Beispielausgabe:
```
Test bestanden.
```

### Verwenden von `bats-core` zum Testen:
Installieren Sie zuerst `bats-core`. Dies kann normalerweise über Ihren Paketmanager oder durch Klonen seines Repositories geschehen.

Dann schreiben Sie Ihre Tests in separate `.bats`-Dateien.

```bash
# Datei: example_function.bats

#!/usr/bin/env bats

@test "test example function" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
Um Ihre Tests auszuführen, führen Sie einfach die `.bats`-Datei aus:
```bash
bats example_function.bats
```
Beispielausgabe:
```
 ✓ test example function

1 Test, 0 Fehler
```

Dieser Ansatz ermöglicht es Ihnen, Tests einfach in Ihren Entwicklungsworkflow zu integrieren und somit die Zuverlässigkeit und Stabilität Ihrer Bash-Skripte zu gewährleisten.
