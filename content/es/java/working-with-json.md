---
title:                "Trabajando con json"
html_title:           "Java: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con JSON?

Si estás interesado en el desarrollo de aplicaciones en Java, es probable que en algún momento te encuentres con la necesidad de trabajar con JSON. JSON (JavaScript Object Notation) es un formato ampliamente utilizado para el intercambio de datos en aplicaciones web. Al comprender cómo trabajar con JSON en Java, podrás manipular y almacenar datos de manera eficiente, lo que te permitirá mejorar tus habilidades de programación y desarrollar aplicaciones más robustas y versátiles.

## Cómo hacerlo

Java ofrece varias bibliotecas que facilitan la manipulación de datos JSON. A continuación, se muestra un ejemplo de cómo se puede convertir una cadena JSON en un objeto Java utilizando la biblioteca Gson:

```Java
import com.google.gson.Gson;

// La cadena JSON de ejemplo
String json = "{'nombre': 'Juan', 'edad': 25, 'ciudad': 'Madrid'}";

// Convertir JSON a un objeto Java
Gson gson = new Gson();
Persona persona = gson.fromJson(json, Persona.class);

System.out.println(persona.getNombre()); // Salida: Juan
System.out.println(persona.getEdad()); // Salida: 25
System.out.println(persona.getCiudad()); // Salida: Madrid
```

En este ejemplo, se utiliza la clase Gson de la biblioteca Gson para convertir la cadena JSON en un objeto Java de tipo Persona. Luego, se pueden acceder a los datos del objeto utilizando los métodos correspondientes.

Otra forma de trabajar con JSON en Java es a través de la biblioteca Jackson. Esta también permite convertir fácilmente datos JSON a objetos Java y viceversa. Aquí tienes un ejemplo:

```Java
import com.fasterxml.jackson.databind.ObjectMapper;

// La cadena JSON de ejemplo
String json = "{'marca': 'Nike', 'modelo': 'Air Max', 'precio': 100}";

// Convertir JSON a un objeto Java
ObjectMapper objectMapper = new ObjectMapper();
Zapato zapato = objectMapper.readValue(json, Zapato.class);

System.out.println(zapato.getMarca()); // Salida: Nike
System.out.println(zapato.getModelo()); // Salida: Air Max
System.out.println(zapato.getPrecio()); // Salida: 100
```

En este caso, se utiliza la clase ObjectMapper de la biblioteca Jackson para realizar la conversión. Como puedes ver, ambas bibliotecas ofrecen formas sencillas de trabajar con JSON en Java.

## Profundizando en JSON

Una de las ventajas de trabajar con JSON es su estructura sencilla que lo hace muy legible para los humanos. Está compuesto por una colección de pares clave-valor y puede soportar diferentes tipos de datos como cadenas, números, objetos y matrices.

En Java, los objetos JSON se pueden mapear a clases Java utilizando la anotación @JsonProperty para asignar los nombres de las propiedades JSON a los nombres de los atributos de la clase. Además, se pueden realizar seralización y deserialización de objetos JSON utilizando las clases JsonGenerator y JsonParser.

Otra característica interesante de JSON es que es muy similar a la sintaxis de los objetos JavaScript, por lo que puede ser fácilmente manejado en aplicaciones web. Además, su tamaño compacto lo hace ideal para transferir datos a través de la red.

## Ver también

- Documentación oficial de JSON en Java: https://docs.oracle.com/javaee/7/tutorial/jsonp004.htm
- Documentación oficial de la biblioteca Gson: https://github.com/google/gson
- Documentación oficial de la biblioteca Jackson: https://github.com/FasterXML/jackson