---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:40.911415-07:00
description: "Come fare: In Java, puoi lavorare con file YAML utilizzando librerie\
  \ di terze parti poich\xE9 l'Edizione Standard di Java non include supporto integrato\
  \ per\u2026"
lastmod: '2024-03-13T22:44:43.329241-06:00'
model: gpt-4-0125-preview
summary: "In Java, puoi lavorare con file YAML utilizzando librerie di terze parti\
  \ poich\xE9 l'Edizione Standard di Java non include supporto integrato per YAML."
title: Lavorare con YAML
weight: 41
---

## Come fare:
In Java, puoi lavorare con file YAML utilizzando librerie di terze parti poiché l'Edizione Standard di Java non include supporto integrato per YAML. Una libreria popolare è SnakeYAML, che permette di analizzare e generare facilmente dati YAML.

### Configurazione di SnakeYAML
Prima, includi SnakeYAML nel tuo progetto. Se stai usando Maven, aggiungi la seguente dipendenza al tuo `pom.xml`:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### Lettura YAML
```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class ReadYamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = ReadYamlExample.class
                .getClassLoader()
                .getResourceAsStream("config.yml")) {
            Map<String, Object> data = yaml.load(inputStream);
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Assumendo che `config.yml` sia così:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
L'output sarà:
```
{name=Example, version=1.0, features=[login, signup]}
```

### Scrittura YAML
Per generare un YAML da oggetti Java, usa il metodo `dump` fornito da SnakeYAML:
```java
import org.yaml.snakeyaml.Yaml;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

public class WriteYamlExample {
    public static void main(String[] args) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", "Example");
        data.put("version", 1.0);
        data.put("features", Arrays.asList("login", "signup"));

        Yaml yaml = new Yaml();
        String output = yaml.dump(data);
        System.out.println(output);
    }
}
```
Questo genererà e stamperà il seguente contenuto YAML:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
Sfruttando SnakeYAML, gli sviluppatori Java possono facilmente integrare l'analisi e la generazione di YAML nelle loro applicazioni, beneficiando della leggibilità e semplicità di YAML per scopi di configurazione e scambio di dati.
