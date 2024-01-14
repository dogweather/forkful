---
title:                "C#: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Por qué trabajar con JSON?

En la actualidad, el uso de JSON en el desarrollo de aplicaciones es indispensable ya que permite transmitir y almacenar datos de manera eficiente. Además, su sintaxis simple y legible lo hace fácil de entender y manipular, lo que lo convierte en una herramienta muy útil en el mundo de la programación.

## Cómo trabajar con JSON

Para trabajar con JSON en C#, se recomienda utilizar la biblioteca Newtonsoft.Json, la cual proporciona una serie de métodos y funciones que facilitan la manipulación de datos en formato JSON.

Primero, es necesario importar la biblioteca en nuestro proyecto utilizando el gestor de paquetes NuGet:

```C#
Install-Package Newtonsoft.Json
```

Una vez importada la biblioteca, podemos empezar a trabajar con JSON. Por ejemplo, si queremos convertir un objeto en formato JSON a una cadena de texto, utilizamos el método SerializeObject:

```C#
//Objeto en formato JSON
string jsonString = JsonConvert.SerializeObject(objeto);

//Imprimir la cadena de texto resultante
Console.WriteLine(jsonString);

/* Output:
"{"nombre":"Juan", "edad": 25}"
*/
```

De igual manera, si queremos convertir una cadena de texto en formato JSON a un objeto, utilizamos el método DeserializeObject:

```C#
//Cadena de texto en formato JSON
string jsonString = "{\"nombre\":\"Juan\", \"edad\": 25}";

//Convertir a objeto
var objeto = JsonConvert.DeserializeObject(jsonString);

//Acceder a los datos del objeto
Console.WriteLine(objeto.nombre); //Juan
Console.WriteLine(objeto.edad); //25
```

También es posible trabajar con archivos JSON. Para ello, podemos utilizar los métodos Load y Save de la clase File de Newtonsoft.Json:

```C#
//Cargar archivo JSON
string jsonString = File.ReadAllText("datos.json");

//Convertir a objeto
var objeto = JsonConvert.DeserializeObject(jsonString);

//Modificar datos del objeto
objeto.edad = 26;

//Guardar cambios en el archivo
File.WriteAllText("datos.json", JsonConvert.SerializeObject(objeto));
```

## Profundizando en JSON

Como se mencionó anteriormente, la sintaxis de JSON es muy simple y legible, lo que lo hace fácil de entender y manipular. A continuación, se presentan algunos conceptos clave que son importantes conocer al trabajar con JSON en C#.

- JSON tiene una estructura de datos basada en pares clave-valor, es decir, cada dato está asociado a una clave única que lo identifica.
- Los valores en JSON pueden ser de diferentes tipos de datos como cadenas de texto, números, objetos, arreglos, entre otros.
- Los datos en JSON se encuentran entre llaves { } y se separan por comas.
- En C#, los objetos en formato JSON se convierten en instancias de la clase JObject, mientras que los arreglos se convierten en instancias de la clase JArray.
- Para acceder a los datos en un objeto o arreglo JSON, se utiliza la sintaxis de punto (.) y corchetes ([ ]).

Estos son solo algunos conceptos básicos de JSON, pero existen muchos más que se pueden explorar para sacarle el máximo provecho a esta útil herramienta en tus proyectos de programación en C#.

## Ver también

- [Documentación oficial de Newtonsoft.Json](https://www.newtonsoft.com/json/help/html/Introduction.htm)
- [Tutorial de JSON en C#](https://www.c-sharpcorner.com/article/working-with-json-in-C-Sharp/)
- [Conversión de objetos en C# a formato JSON y viceversa](https://www.c-sharpcorner.com/article/json-serialization-deserialization-in-net/)