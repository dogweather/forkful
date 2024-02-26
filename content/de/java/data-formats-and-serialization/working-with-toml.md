---
date: 2024-01-26 04:22:45.380058-07:00
description: "TOML steht f\xFCr Toms Offensichtliche, Minimalistische Sprache. Es\
  \ ist ein Daten-Serialisierungsformat, das f\xFCr Konfigurationsdateien verwendet\
  \ wird.\u2026"
lastmod: '2024-02-25T18:49:50.850419-07:00'
model: gpt-4-0125-preview
summary: "TOML steht f\xFCr Toms Offensichtliche, Minimalistische Sprache. Es ist\
  \ ein Daten-Serialisierungsformat, das f\xFCr Konfigurationsdateien verwendet wird.\u2026"
title: Arbeiten mit TOML
---

{{< edit_this_page >}}

## Was & Warum?
TOML steht für Toms Offensichtliche, Minimalistische Sprache. Es ist ein Daten-Serialisierungsformat, das für Konfigurationsdateien verwendet wird. Programmierer nutzen es, weil es leicht zu lesen, zu schreiben ist und sich gut auf eine Hash-Tabelle abbilden lässt.

## Wie geht das:
Du benötigst eine TOML-Parsing-Bibliothek. Ich empfehle `toml4j`. Füge sie so deinem Projekt hinzu:

```java
// Füge das zu deiner build.gradle hinzu
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

So parst du eine TOML-Datei:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("Server-IP: " + ip);
        System.out.println("Server-Port: " + port);
    }
}
```

Beispielausgabe:

```
Server-IP: 192.168.1.1
Server-Port: 80
```

## Vertiefung
Entwickelt von GitHub-Mitbegründer Tom Preston-Werner, zielte TOML darauf ab, einfacher als XML und spezifizierter als YAML zu sein. Seine neueste Version 1.0.0, veröffentlicht im Jahr 2021, bietet einen stabilen Satz von Funktionen.

Alternativen wie JSON oder YAML sind ebenfalls beliebt. JSON ist großartig für den Datenaustausch. YAML ist für komplexe Konfigurationen menschenlesbarer. TOMLs Stärke liegt in seiner Geradlinigkeit und seiner Nutzung in der Rust-Community.

Was die Implementierung angeht, wenn du TOML mit Java verwendest, beachte, dass der von dir gewählte Parser eine Rolle spielt. Neben `toml4j` greifen einige auf `jackson-dataformat-toml` zurück. Jeder wird seine Nuancen haben, wie Fehlerbehandlung oder Parsing-Leistung, also wähle basierend auf den Bedürfnissen deines Projekts.

## Siehe auch
- TOML-Spezifikation: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
