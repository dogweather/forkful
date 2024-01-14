---
title:                "Javascript: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

##Por qué

Si estás buscando una forma fácil y legible de almacenar y transmitir datos en tus proyectos de Javascript, entonces trabajar con YAML es la respuesta. YAML es un formato de datos que es fácil de entender para los humanos y también es compatible con una amplia gama de lenguajes de programación.

##Cómo hacerlo

Para comenzar a trabajar con YAML en Javascript, primero debes asegurarte de tener una biblioteca que te permita analizar y manipular archivos YAML. Una opción popular es la librería "js-yaml", que puedes instalar utilizando npm. Luego, puedes importar la librería en tu archivo de Javascript utilizando la siguiente sintaxis:

```Javascript
const yaml = require('js-yaml');
```

Una vez que tienes la librería a tu disposición, puedes empezar a trabajar con datos YAML. Por ejemplo, si tienes un archivo YAML llamado "datos.yml", con la siguiente estructura:

```YAML
nombre: Juan
edad: 30
hobbies:
  - Jugar videojuegos
  - Leer libros
```

Puedes acceder a la información almacenada en este archivo utilizando la función "safeLoad" de la librería js-yaml. Por ejemplo:

```Javascript
var datos = yaml.safeLoad('datos.yml');
console.log(datos.nombre); // imprime "Juan"
console.log(datos.edad); // imprime 30
console.log(datos.hobbies[1]); // imprime "Leer libros"
```

Además de acceder a los datos, también puedes modificarlos y guardarlos en un archivo YAML utilizando la función "safeDump" de la librería. Por ejemplo:

```Javascript
datos.edad = 31;
datos.hobbies.push("Cocinar");
yaml.safeDump(datos, 'datos.yml'); // guarda la información modificada en el archivo YAML
```

##Profundizando

Si deseas aprender más sobre cómo utilizar YAML en tus proyectos de Javascript, hay muchas funciones y opciones disponibles en la librería js-yaml. Puedes explorar la documentación para encontrar la estructura adecuada para tu proyecto y descubrir cómo puedes aprovechar al máximo este formato de datos.

Además, también puedes buscar y aprender sobre otros formatos de datos, como JSON y XML, y comparar sus pros y contras con YAML. Esto puede ayudarte a tomar una decisión informada sobre qué formato de datos utilizar en tu proyecto.

##Ver también

- [Documentación de js-yaml](https://www.npmjs.com/package/js-yaml)
- [Comparación entre YAML, JSON y XML](https://phil.tech/2019/comparing-data-serialization-formats-json-protobuf-flatbuffers-pickle-hessian-avro-xml/)