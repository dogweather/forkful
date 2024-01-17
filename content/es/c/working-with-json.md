---
title:                "Trabajando con json"
html_title:           "C: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Trabajar con JSON es una técnica común en la programación moderna. JSON, que significa JavaScript Object Notation, es un formato de intercambio de datos ligero y legible por humanos. Los programadores lo utilizan para almacenar y transmitir datos de manera eficiente entre aplicaciones.

## Cómo:

Aquí te mostramos cómo trabajar con JSON en C:

```
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <jansson.h>

int main()
{
	// Ejemplo de datos en formato JSON
	const char *datos = "{\"nombre\":\"Juan\",\"apellido\":\"García\",\"edad\":25}";

	// Crear un objeto JSON a partir de los datos
	json_t *objeto = json_loads(datos, 0, NULL);

	// Obtener valores individuales del objeto
	const char *nombre = json_string_value(json_object_get(objeto, "nombre"));
	const char *apellido = json_string_value(json_object_get(objeto, "apellido"));
	int edad = json_integer_value(json_object_get(objeto, "edad"));

	// Imprimir los datos obtenidos
	printf("Nombre: %s\n", nombre);
	printf("Apellido: %s\n", apellido);
	printf("Edad: %d\n", edad);

	// Liberar la memoria del objeto
	json_decref(objeto);

	return 0;
}
```

Output:

```
Nombre: Juan
Apellido: García
Edad: 25
```

## Profundizando:

Antes de la aparición de JSON, el formato más utilizado para intercambiar datos entre aplicaciones era XML. Sin embargo, XML es mucho más complejo y difícil de leer y escribir para los programadores. JSON, por otro lado, utiliza un formato de pares de clave-valor simples que es más sencillo de entender y procesar.

Aunque JSON es el formato más comúnmente utilizado, existen alternativas como YAML y CSV. Sin embargo, en la mayoría de los casos, JSON sigue siendo la opción preferida debido a su simplicidad y flexibilidad.

En C, trabajar con JSON requiere el uso de una biblioteca externa, como jansson o cJSON. Estas bibliotecas proporcionan funciones y macros útiles para cargar, manipular y crear objetos JSON.

## Ver También:

- [Documentación de jansson](https://jansson.readthedocs.io/en/v2.11/)
- [Documentación de cJSON](https://github.com/DaveGamble/cJSON/blob/master/README.md)
- [Introducción a JSON](https://www.json.org/json-es.html)