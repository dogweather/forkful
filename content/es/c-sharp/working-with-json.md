---
title:                "Trabajando con json"
html_title:           "C#: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con JSON?

Si eres un programador de C#, es muy probable que en algún momento te encuentres trabajando con JSON. Este formato de datos se ha vuelto muy popular debido a su simplicidad y flexibilidad, lo que lo convierte en una herramienta poderosa para el intercambio de información en aplicaciones web y móviles.

## Cómo trabajar con JSON en C#

La librería *Newtonsoft.Json* es la opción más común para trabajar con JSON en C#. Puedes agregarla a tu proyecto a través del administrador de paquetes NuGet. Una vez agregada, puedes utilizar la clase `JObject` para leer y manipular objetos JSON. A continuación, se muestra un ejemplo de cómo leer y acceder a los datos de un archivo JSON:

```
using Newtonsoft.Json;

// Lectura del archivo JSON
string jsonString = File.ReadAllText("datos.json");

// Convertir el JSON en un objeto
JObject jsonObj = JObject.Parse(jsonString);

// Acceder a la propiedad "nombre"
string nombre = (string)jsonObj["nombre"];
```

También puedes utilizar la clase `JArray` para trabajar con arrays JSON y la clase `JValue` para acceder y convertir valores individuales. Puedes encontrar más detalles sobre cómo trabajar con JSON en la documentación oficial de *Newtonsoft.Json*.

## Profundizando en el trabajo con JSON

Además de leer y escribir objetos y arrays JSON, también puedes ser más específico en tu manipulación de datos utilizando consultas LINQ y métodos como `SelectToken()` y `ToObject()`. También puedes personalizar la deserialización de objetos utilizando atributos de la librería *Newtonsoft.Json*. Ten en cuenta que, aunque JSON es una forma popular de intercambiar datos, aún debes tomar precauciones para validar y asegurar los datos recibidos antes de utilizarlos en tu aplicación.

## Ver también

- Documentación oficial de *Newtonsoft.Json*:  https://www.newtonsoft.com/json/help/html/Introduction.htm
- Tutorial de C# y JSON en C# Corner: https://www.c-sharpcorner.com/article/c-sharp-amd-json-web-service-communication/