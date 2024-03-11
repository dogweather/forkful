---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:57.859873-07:00
description: "Regul\xE4re Ausdr\xFCcke (regex) sind Zeichenfolgen, die Suchmuster\
  \ definieren, die haupts\xE4chlich f\xFCr die Zeichenkettenabgleichung und -manipulation\
  \ verwendet\u2026"
lastmod: '2024-03-11T00:14:28.035137-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE4re Ausdr\xFCcke (regex) sind Zeichenfolgen, die Suchmuster definieren,\
  \ die haupts\xE4chlich f\xFCr die Zeichenkettenabgleichung und -manipulation verwendet\u2026"
title: "Regul\xE4re Ausdr\xFCcke verwenden"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke (regex) sind Zeichenfolgen, die Suchmuster definieren, die hauptsächlich für die Zeichenkettenabgleichung und -manipulation verwendet werden. Programmierer nutzen regex in Arduino-Projekten, um serielle Eingaben zu parsen, Benutzereingaben zu validieren oder Daten aus Zeichenketten zu extrahieren, was die Effizienz und Flexibilität der Datenverarbeitung steigert.

## Wie:
Arduino hat keine eingebaute Unterstützung für regex direkt in seiner Standardbibliothek. Sie können jedoch regex-ähnliche Funktionalitäten für einfache Muster mit grundlegenden Zeichenkettenfunktionen erreichen oder für komplexere Bedürfnisse eine Drittanbieterbibliothek wie `regex` integrieren.

### Grundlegende Zeichenkettenabgleichung ohne Regex
Für grundlegende Bedürfnisse, wie das Finden einer Teilzeichenkette, können Sie die Funktion `String.indexOf()` verwenden:
```cpp
String data = "Sensorwert: 12345";
int index = data.indexOf("Wert:");
if (index != -1) {
  String value = data.substring(index + 6).trim();
  Serial.println(value); // Gibt aus: 12345
}
```

### Verwendung einer Drittanbieterbibliothek für Regex
Um komplexere Muster zu behandeln, könnten Sie eine Bibliothek wie `regex` in Betracht ziehen. Nach der Installation der Bibliothek können Sie sie wie folgt verwenden:

1. **Installation**: Die `regex`-Bibliothek ist möglicherweise nicht direkt im Arduino-Bibliotheksmanager verfügbar, daher müssen Sie sie möglicherweise manuell installieren, indem Sie sie von einer seriösen Quelle herunterladen und zu Ihrem Arduino-Bibliotheksordner hinzufügen.

2. **Beispielverwendung**:
Angenommen, die Bibliothek bietet Funktionen ähnlich den Standard-regex-Implementierungen, könnten Sie sie wie folgt verwenden:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // Warten, bis Serial bereit ist
  
  regex_t reg;
  const char* pattern = "[0-9]+"; // Passt auf eine Folge von Ziffern
  regcomp(&reg, pattern, REG_EXTENDED);
  
  const char* test_str = "Sensorwert: 12345";
  
  regmatch_t matches[1];
  if (regexec(&reg, test_str, 1, matches, 0) == 0) {
    // Extrahieren und ausgeben des passenden Teils
    int start = matches[0].rm_so;
    int end = matches[0].rm_eo;
    char match[end-start+1];
    strncpy(match, test_str + start, end-start);
    match[end-start] = '\0';
    
    Serial.print("Passende Übereinstimmung gefunden: ");
    Serial.println(match); // Gibt aus: 12345
  } else {
    Serial.println("Keine Übereinstimmung gefunden");
  }
  
  regfree(&reg); // Den für regex allokierten Speicher freigeben
}

void loop() {
  // setze hier deinen Hauptcode ein, um ihn wiederholt auszuführen:
}
```

**Hinweis**: Die Syntax und spezifischen Funktionen, die hier verwendet werden, dienen nur zu Veranschaulichungszwecken und können je nach den tatsächlichen Implementierungsdetails der `regex`-Bibliothek, die Sie wählen, variieren. Beziehen Sie sich immer auf die Dokumentation der Bibliothek für genaue und aktuelle Informationen.
