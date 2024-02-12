---
title:                "Trabajando con JSON"
date:                  2024-02-03T19:23:17.319349-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?
Trabajar con JSON (Notación de Objetos de JavaScript) significa manejar este formato ligero de intercambio de datos dentro de tus aplicaciones Java. Los programadores optan por JSON para serializar y transmitir datos estructurados a través de una red y configurar y almacenar datos fácilmente porque es legible por humanos e independiente del lenguaje.

## Cómo hacerlo:
Vamos a remangarnos y empezar a codificar con JSON en Java.

Primero que nada, necesitarás una biblioteca de procesamiento de JSON como `Jackson` o `Google Gson`. Aquí usaremos `Jackson`, así que añade esta dependencia a tu `pom.xml`:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Ahora, vamos a serializar (escribir) un simple objeto Java a JSON:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class EjemploJson {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Persona persona = new Persona("Alex", 30);
            String json = mapper.writeValueAsString(persona);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Persona {
    public String nombre;
    public int edad;

    public Persona(String nombre, int edad) {
        this.nombre = nombre;
        this.edad = edad;
    }
}
```

La salida debería ser:

```json
{"nombre":"Alex","edad":30}
```

Ahora, para deserializar (leer) JSON de vuelta a un objeto Java:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class EjemploJson {
    public static void main(String[] args) {
        String json = "{\"nombre\":\"Alex\",\"edad\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Persona persona = mapper.readValue(json, Persona.class);
            System.out.println(persona.nombre + " tiene " + persona.edad + " años.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

La salida será:

```
Alex tiene 30 años.
```

## Profundización
La simplicidad y eficacia de JSON han hecho que se convierta en el estándar de facto para el intercambio de datos en la web, desbancando a XML de su trono. Introducido a principios de los años 2000, JSON se derivó de JavaScript pero ahora es compatible con la mayoría de los lenguajes.

Las alternativas a JSON incluyen XML, que es más verboso, y formatos binarios como Protocol Buffers o MessagePack, que son menos legibles por humanos pero más eficientes en tamaño y velocidad. Cada uno tiene sus casos de uso; la elección depende de tus necesidades específicas de datos y contexto.

En Java, más allá de `Jackson` y `Gson`, tenemos `JsonB` y `org.json` como otras bibliotecas para manejar JSON. Jackson ofrece procesamiento basado en flujos y es conocido por su velocidad, mientras que Gson es celebrado por su facilidad de uso. JsonB es parte de Jakarta EE, ofreciendo un enfoque más estandarizado.

Cuando implementes JSON, recuerda manejar tus excepciones correctamente - tu código debe ser robusto contra entradas erróneas. ¡También, considera las implicaciones de seguridad del enlace de datos automático – siempre valida tus entradas!

## Ver También
- [Proyecto Jackson](https://github.com/FasterXML/jackson)
- [Proyecto Gson](https://github.com/google/gson)
- [Especificación de JSON](https://www.json.org/json-en.html)
- [Especificación de JsonB](https://jakarta.ee/specifications/jsonb/)
