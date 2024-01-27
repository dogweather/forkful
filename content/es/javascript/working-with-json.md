---
title:                "Trabajando con JSON"
date:                  2024-01-19
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
JSON significa JavaScript Object Notation. Es un formato ligero para intercambiar datos. Programadores lo usan porque es fácil de leer y escribir para humanos, al mismo tiempo que es sencillo para las máquinas parsear y generar.

## Cómo Hacerlo:
Trabajando con JSON en JavaScript es sencillo. Ejemplos abajo:

### Convertir un objeto a JSON:
```javascript
const objeto = {nombre: "Juan", edad: 30, ciudad: "Madrid"};
const json = JSON.stringify(objeto);
console.log(json);
// Salida: '{"nombre":"Juan","edad":30,"ciudad":"Madrid"}'
```

### Convertir JSON a objeto:
```javascript
const json = '{"nombre":"Juan","edad":30,"ciudad":"Madrid"}';
const objeto = JSON.parse(json);
console.log(objeto);
// Salida: {nombre: "Juan", edad: 30, ciudad: "Madrid"}
```

### Manejo de errores al parsear:
```javascript
const jsonErroneo = '{"nombre":"Juan",edad:"30"}'; // Falta comillas en "edad"
try {
  const objeto = JSON.parse(jsonErroneo);
} catch(err) {
  console.error('Error al parsear JSON:', err.message);
}
// Salida: Error al parsear JSON: Unexpected token e in JSON at position 20
```

## Mirada en Profundidad:
JSON fue propuesto por Douglas Crockford a principios de los 2000, simplificando la manera de intercambiar datos. XML fue una alternativa pero JSON ganó terreno por su simplicidad y eficiencia en cuanto a la rapidez y el tamaño del dato.

Es vital entender que JSON es un formato de texto y puede ser utilizado con cualquier lenguaje de programación, no sólo JavaScript. Aunque casi todos los navegadores y servidores lo manejan perfectamente, siempre es bueno realizar validaciones para asegurarnos de que el JSON es correcto antes de usarlo en nuestras aplicaciones.

## Ver También:
Aquí tienes algunos recursos útiles para profundizar tu entendimiento y habilidades trabajando con JSON:

- [JSON.org](https://www.json.org/json-es.html): Documentación oficial de JSON.
- [MDN Web Docs - JSON](https://developer.mozilla.org/es/docs/Learn/JavaScript/Objects/JSON): Guía y referencia de JSON en MDN Web Docs.
- [w3schools – JSON Tutorial](https://www.w3schools.com/js/js_json_intro.asp): Tutorial de JSON para principiantes.

Estos links pueden llevarte de entender lo básico a manejar JSON como un experto. ¡Buena suerte codificando!
