---
title:                "Javascript: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Por qué trabajar con JSON

El formato de Notación de Objetos Javascript, mejor conocido como JSON, se ha vuelto cada vez más popular en el mundo del desarrollo web. Es una forma fácil y eficiente de almacenar y transmitir datos entre un servidor y una aplicación web. Si quieres aprender a utilizar JSON en tus proyectos, sigue leyendo.

## Cómo utilizar JSON en Javascript

Para empezar a trabajar con JSON, lo primero que debes hacer es declarar una variable y asignarle los datos en formato JSON. Puedes hacerlo de la siguiente manera:

```Javascript
var datos = { 
  "nombre": "Juan",
  "edad": 25,
  "ciudad": "Madrid"
};
```

En este ejemplo, la variable "datos" contiene tres pares de clave-valor en formato JSON. La clave es el nombre del dato y el valor es el contenido del mismo. Ahora, para acceder a un dato específico, puedes utilizar la sintaxis punto o corchetes:

```Javascript
console.log(datos.nombre); // output: Juan
console.log(datos["edad"]); // output: 25
```

También puedes utilizar bucles y condicionales para recorrer y manipular los datos en formato JSON. Por ejemplo, puedes recorrer un arreglo de objetos JSON y mostrar su contenido en una lista en tu página web:

```Javascript
var list = document.getElementById("lista");

for (var i = 0; i < datos.length; i++) {
  var li = document.createElement("li");
  li.innerHTML = datos[i].nombre + " - " + datos[i].ciudad;
  list.appendChild(li); 
}

/* Output:
- Juan - Madrid
- Elena - Barcelona 
- Carlos - Valencia
*/
```

## Profundizando en JSON

JSON ofrece una gran variedad de funcionalidades para trabajar con datos en formato de objetos. Puedes utilizar métodos como "JSON.stringify()" para convertir un objeto Javascript en formato JSON y viceversa con "JSON.parse()". También puedes utilizar la función "JSON.parse()" para validar si un string está en formato JSON válido.

Otra ventaja de utilizar JSON en tus proyectos es que es fácil de leer para los seres humanos y también para las máquinas. Esto hace que sea un formato versátil y ampliamente utilizado en aplicaciones web y móviles.

## Ver también

- [Documentación oficial de JSON en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [Tutorial de W3Schools sobre JSON en Javascript](https://www.w3schools.com/js/js_json_intro.asp)
- [Introducción a JSON en el blog de Platzi](https://platzi.com/blog/introduccion-json/)
- [Curso de Javascript en Platzi para aprender más sobre este y otros temas](https://platzi.com/cursos/javascript/)