---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:33.348233-07:00
description: "C\xF3mo: En Java, puedes trabajar con archivos YAML utilizando bibliotecas\
  \ de terceros, ya que la Edici\xF3n Est\xE1ndar de Java no incluye soporte incorporado\u2026"
lastmod: '2024-03-13T22:44:58.957641-06:00'
model: gpt-4-0125-preview
summary: "En Java, puedes trabajar con archivos YAML utilizando bibliotecas de terceros,\
  \ ya que la Edici\xF3n Est\xE1ndar de Java no incluye soporte incorporado para YAML."
title: Trabajando con YAML
weight: 41
---

## Cómo:
En Java, puedes trabajar con archivos YAML utilizando bibliotecas de terceros, ya que la Edición Estándar de Java no incluye soporte incorporado para YAML. Una biblioteca popular es SnakeYAML, que permite analizar y generar datos YAML fácilmente.

### Configurando SnakeYAML
Primero, incluye SnakeYAML en tu proyecto. Si estás utilizando Maven, agrega la siguiente dependencia a tu archivo `pom.xml`:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### Leyendo YAML
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
Asumiendo que `config.yml` tiene el siguiente aspecto:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
La salida será:
```
{name=Example, version=1.0, features=[login, signup]}
```

### Escribiendo YAML
Para generar un YAML a partir de objetos Java, utiliza el método `dump` proporcionado por SnakeYAML:
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
Esto generará e imprimirá el siguiente contenido YAML:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
Al aprovechar SnakeYAML, los desarrolladores de Java pueden integrar fácilmente el análisis y la generación de YAML en sus aplicaciones, beneficiándose de la legibilidad y simplicidad de YAML para fines de configuración e intercambio de datos.
