---
title:                "Trabajando con JSON"
date:                  2024-01-19
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con JSON (JavaScript Object Notation) significa manipular datos en un formato ligero de intercambio. Programadores lo usan por su facilidad de entendimiento humano y su integración sencilla con la mayoría de lenguajes de programación, incluido Java.

## Cómo Hacerlo:

```Java
import org.json.JSONObject; // Asegúrate de incluir la biblioteca org.json

public class EjemploJSON {
    public static void main(String[] args) {
        // Crear un objeto JSON
        JSONObject miObjetoJSON = new JSONObject();
        miObjetoJSON.put("nombre", "Carlos");
        miObjetoJSON.put("edad", 25);
        miObjetoJSON.put("programador", true);

        // Convertir y mostrar el objeto JSON como cadena
        String jsonString = miObjetoJSON.toString();
        System.out.println(jsonString);

        // Leer datos de un objeto JSON
        String nombre = miObjetoJSON.getString("nombre");
        System.out.println("Nombre: " + nombre);
    }
}
```

Salida de muestra:
```
{"nombre":"Carlos","edad":25,"programador":true}
Nombre: Carlos
```

## Profundizando

JSON fue introducido en 2001, inspirado en la notación de objetos de JavaScript pero independiente del lenguaje. Alternativas incluyen XML y YAML, pero JSON gana en simplicidad y velocidad. Cuando trabajas con Java, algunas implementaciones populares para manejar JSON son las bibliotecas `org.json`, `Gson` de Google y `Jackson`.

## Ver También

- Documentación oficial de la biblioteca org.json: https://stleary.github.io/JSON-java/index.html
- Google Gson: https://github.com/google/gson
- Jackson JSON processor: https://github.com/FasterXML/jackson
- Tutorial completo de JSON en Java: https://www.baeldung.com/java-org-json
