---
title:                "Trabajando con json"
html_title:           "Javascript: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con JSON es una práctica común en la programación moderna. JSON, o Notación de Objetos de JavaScript, es un formato de intercambio de datos ligero y fácil de leer que se utiliza para almacenar y transmitir información estructurada. Los programadores utilizan JSON para comunicarse con diferentes sistemas y aplicaciones, ya que es un formato estándar y altamente compatible.

## Cómo:

```Javascript
// Creación de un objeto JSON
const persona = {
  nombre: 'María',
  edad: 25,
  ciudad: 'Madrid'
};

// Codificación de un objeto JSON en una cadena
const jsonString = JSON.stringify(persona);
console.log(jsonString); //{"nombre": "María", "edad": 25, "ciudad": "Madrid"}

// Decodificación de una cadena JSON en un objeto
const json = '{"nombre": "Juan", "edad": 30, "ciudad": "Barcelona"}';
const personaDecodificada = JSON.parse(json);

// Acceder a los valores de un objeto JSON
console.log(personaDecodificada.nombre); //Juan
console.log(personaDecodificada.edad); //30
```

## Profundizando:

JSON fue diseñado originalmente por Douglas Crockford en 2002 y es ampliamente utilizado en aplicaciones web y móviles. Aunque es muy popular, hay alternativas como XML o CSV que también se utilizan para intercambiar datos. La implementación de JSON en JavaScript se realiza a través de los métodos `JSON.stringify()` y `JSON.parse()`, los cuales convierten objetos JavaScript en cadena JSON y viceversa.

## Ver también:

- [Documentación oficial de JSON](https://www.json.org/)
- [Tutorial de JSON en W3Schools](https://www.w3schools.com/js/js_json_intro.asp)