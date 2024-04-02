---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:55.129934-07:00
description: "Los arrays asociativos, conocidos como objetos en Google Apps Script\
  \ (una variante de JavaScript), permiten a los programadores crear colecciones de\
  \ pares\u2026"
lastmod: '2024-03-13T22:44:58.516667-06:00'
model: gpt-4-0125-preview
summary: "Los arrays asociativos, conocidos como objetos en Google Apps Script (una\
  \ variante de JavaScript), permiten a los programadores crear colecciones de pares\u2026"
title: Utilizando arreglos asociativos
weight: 15
---

## ¿Qué y por qué?

Los arrays asociativos, conocidos como objetos en Google Apps Script (una variante de JavaScript), permiten a los programadores crear colecciones de pares clave-valor. Esta funcionalidad es fundamental para almacenar y manipular datos de manera eficiente, especialmente cuando se trabaja con propiedades con nombres dinámicos o cuando el modelo de almacenamiento y acceso lineal de un array tradicional es insuficiente.

## Cómo:

En Google Apps Script, se crean y manipulan arrays asociativos (objetos) usando llaves `{}`, definiendo pares clave-valor dentro. Las claves son identificadores únicos y los valores pueden ser cualquier cosa, desde cadenas y números hasta objetos más complejos o funciones. Aquí hay un ejemplo básico:

```javascript
function createAssociativeArray() {
  var usuario = {
    nombre: "John Doe",
    edad: 30,
    correo: "johndoe@example.com"
  };

  // Accediendo a los valores
  Logger.log(usuario.nombre); // Muestra: John Doe
  Logger.log(usuario["correo"]); // Muestra: johndoe@example.com

  // Añadiendo nuevos pares clave-valor
  usuario.titulo = "Desarrollador de Software";
  usuario["país"] = "EEUU";

  Logger.log(usuario.titulo); // Muestra: Desarrollador de Software

  // Iterando sobre los pares clave-valor
  for (var clave in usuario) {
    Logger.log(clave + ': ' + usuario[clave]);
  }
}
```

La salida de muestra para la parte de iteración podría verse así:
```
nombre: John Doe
edad: 30
correo: johndoe@example.com
titulo: Desarrollador de Software
país: EEUU
```

Note cómo se puede usar tanto la notación por puntos como la notación de corchetes para acceder y establecer propiedades. La notación de corchetes es particularmente útil cuando se trabaja con claves que se determinan de manera dinámica o incluyen caracteres no permitidos en identificadores.

## Análisis Profundo

Los arrays asociativos en forma de objetos han sido una piedra angular de JavaScript, y por extensión de Google Apps Script, reflejando su mecanismo de herencia basado en prototipos. A diferencia de los lenguajes con arrays asociativos tradicionales o diccionarios (por ejemplo, el dict de Python), los objetos de Google Apps Script proporcionan un medio flexible y poderoso para estructurar datos, beneficiándose de la naturaleza dinámica de JavaScript.

Es importante señalar, sin embargo, que la especificación de ECMAScript 2015 introdujo los objetos `Map` y `Set`, ofreciendo un manejo de colecciones asociativas más directo con ciertas ventajas sobre los objetos, como mantener el orden de inserción y un mejor rendimiento para grandes conjuntos de datos. Aunque Google Apps Script también admite estos, la elección entre usar objetos o las estructuras `Map`/`Set` más nuevas depende de necesidades específicas y consideraciones de rendimiento. Para la mayoría de las tareas de arrays asociativos, las implementaciones basadas en objetos tradicionales proporcionan un enfoque familiar y versátil, pero es aconsejable examinar alternativas más recientes a medida que la complejidad de su script aumenta.
