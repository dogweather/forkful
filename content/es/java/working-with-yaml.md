---
title:                "Trabajando con YAML"
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Trabajar con YAML significa manejar archivos `.yaml` o `.yml`, populares por ser fáciles de leer para humanos. Programadores los usan para configuraciones, debido a su claridad y simplicidad.

## Cómo Hacerlo:
Para trabajar con YAML en Java, necesitamos una biblioteca; `SnakeYAML` es común. Aquí hay un ejemplo de cómo leer y escribir YAML con esta librería.

```java
import org.yaml.snakeyaml.Yaml;
import java.util.Map;

public class EjemploYAML {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();

        // Leer YAML desde un string
        String documento = "titulo: Ejemplo YAML\n" +
                           "lista: \n" +
                           "  - item1\n" +
                           "  - item2\n";
        Map<String, Object> data = yaml.load(documento);
        System.out.println(data);

        // Escribir un objeto a YAML
        Map<String, String> config = Map.of("clave", "valor", "otraClave", "otroValor");
        String salidaYAML = yaml.dump(config);
        System.out.println(salidaYAML);
    }
}
```

Output al leer YAML:
```java
{titulo=Ejemplo YAML, lista=[item1, item2]}
```

Output al escribir YAML:
```java
{clave=valor, otraClave=otroValor}
```

## Análisis Profundo:
YAML, que significa "YAML Ain't Markup Language" (un juego de palabras que indica que no es un lenguaje de marcado), surgió en 2001. Jackson y org.yaml:snakeyaml son dos bibliotecas Java populares para trabajar con YAML. Aunque JSON y XML son alternativas, YAML es preferido cuando la legibilidad es crucial. La implementación típica de Java para YAML implica mapear datos entre POJOs (Plain Old Java Objects) y archivos YAML.

## Consulta También:
- Documentación de `SnakeYAML`: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- Tutorial para Jackson y YAML: https://www.baeldung.com/jackson-yaml
- Especificación oficial de YAML: https://yaml.org/spec/1.2/spec.html