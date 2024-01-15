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

## ¿Por qué trabajar con JSON?

Trabajar con JSON en JavaScript es una práctica común en el desarrollo web moderno. JSON (JavaScript Object Notation) es un formato de intercambio de datos ligero y legible para los humanos, que se ha vuelto muy popular gracias a su fácil sintaxis y su compatibilidad con múltiples lenguajes de programación.

## Cómo hacerlo

Para trabajar con JSON en JavaScript, hay algunos pasos que debes seguir:

1. Primero, asegúrate de tener un objeto JavaScript que quieras convertir a JSON.

Ejemplo de objeto JavaScript:
```Javascript
let persona = {
  nombre: "María",
  edad: 30,
  ciudad: "Madrid"
}
```

2. Utiliza el método `JSON.stringify()` para convertir el objeto a una cadena de texto en formato JSON.

Ejemplo de código:
```Javascript
let personaJson = JSON.stringify(persona);
```

3. Ahora puedes enviar la cadena de texto a través de una solicitud HTTP o almacenarla en un archivo.

Ejemplo de código:
```Javascript
axios.post("https://ejemplo.com/api/persona", personaJson)
```

4. Si quieres trabajar con un JSON recibido de alguna fuente externa, utiliza el método `JSON.parse()` para convertir la cadena de texto a un objeto JavaScript.

Ejemplo de código:
```Javascript
axios.get("https://ejemplo.com/api/persona").then(response => {
  let personaRecibida = JSON.parse(response.data);
});
```

### Más información sobre JSON

Aunque JSON es ampliamente utilizado y su sintaxis es bastante intuitiva, hay algunas cosas a tener en cuenta al trabajar con él en JavaScript:

- JSON solo puede contener datos primitivos (cadenas de texto, números, booleanos) y objetos anidados. No puedes incluir funciones o comentarios en un JSON.
- JSON es muy similar a la sintaxis de un objeto JavaScript, pero con algunas diferencias clave. Por ejemplo, las propiedades en un objeto JSON deben estar entre comillas dobles, mientras que en JavaScript pueden estar sin comillas.
- Puedes validar la sintaxis de un JSON utilizando herramientas en línea como [JSONLint](https://jsonlint.com/) o [JSON Formatter](https://jsonformatter.curiousconcept.com/).

## Profundizando en JSON

Si estás interesado en aprender más sobre JSON, aquí hay algunos temas que podrían ser de tu interés:

- [JSON en la documentación de Mozilla](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/JSON): Aquí encontrarás una guía completa sobre cómo trabajar con JSON en JavaScript, incluyendo métodos y ejemplos.
- [AJAX y JSON](https://www.w3schools.com/js/js_json_intro.asp): Si quieres saber cómo utilizar JSON para hacer solicitudes AJAX, esta guía de W3Schools es un buen lugar para empezar.
- [Validación de JSON con JavaScript](https://www.digitalocean.com/community/tutorials/how-to-parse-json-in-javascript): Este tutorial de DigitalOcean te enseña cómo validar y manejar errores al trabajar con JSON en tu código JavaScript.

## Véase también

- [Introducción a JSON en JavaScript](https://www.youtube.com/watch?v=uU1YUq557Mo)
- [Tutorial de JSON en Codecademy](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-json): Aprende los fundamentos de JSON con este curso interactivo de Codecademy.
- [Documentación oficial de JSON](https://www.json.org/json-es.html): Aquí encontrarás la especificación completa de JSON, así como enlaces a otras herramientas y recursos relacionados.