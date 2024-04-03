---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:13.857282-07:00
description: "La interpolaci\xF3n de cadenas en Google Apps Script permite la incrustaci\xF3\
  n din\xE1mica de expresiones dentro de las cadenas, facilitando la creaci\xF3n de\
  \ c\xF3digo\u2026"
lastmod: '2024-03-13T22:44:58.506242-06:00'
model: gpt-4-0125-preview
summary: "La interpolaci\xF3n de cadenas en Google Apps Script permite la incrustaci\xF3\
  n din\xE1mica de expresiones dentro de las cadenas, facilitando la creaci\xF3n de\
  \ c\xF3digo m\xE1s legible y mantenible."
title: Interpolando una cadena de texto
weight: 8
---

## Cómo hacerlo:
En Google Apps Script, la interpolación de cadenas se logra a través de literales de plantilla. Estos son literales de cadena que permiten expresiones incrustadas, denotados por acentos graves (\`) en lugar de las comunes comillas. Así es como puedes usarlos:

```javascript
// Un ejemplo básico
function ejemploBasicoDeInterpolacion() {
  const usuario = 'Alice';
  console.log(`Hola, ${usuario}!`); // Salida: Hola, Alice!
}

// Usando expresiones
function ejemploInterpolacionConExpresion() {
  const a = 5;
  const b = 10;
  console.log(`Cinco más diez es ${a + b}.`); // Salida: Cinco más diez es 15.
}

// Cadenas de múltiples líneas
function ejemploCadenaMultilinea() {
  const item = 'Google Apps Script';
  console.log(`Esta es una cadena de varias líneas:
Hola a todos,
Hoy estamos discutiendo ${item}.`);
  // Salida:
  // Esta es una cadena de varias líneas:
  // Hola a todos,
  // Hoy estamos discutiendo Google Apps Script.
}

ejemploBasicoDeInterpolacion();
ejemploInterpolacionConExpresion();
ejemploCadenaMultilinea();
```

Estos ejemplos ilustran el uso básico, la incrustación de expresiones y la creación de cadenas de varias líneas con valores interpolados.

## Análisis Profundo
Los literales de plantilla, incluida la interpolación de cadenas, fueron introducidos en ECMAScript 2015 (ES6) y posteriormente adoptados en Google Apps Script. Antes de esto, los programadores tenían que depender puramente de la concatenación de cadenas, lo cual podía volverse engorroso para cadenas complejas o al integrar muchos valores de variables.

```javascript
// Método antiguo (anterior a ES6)
var usuario = 'Bob';
console.log('Hola, ' + usuario + '!');
```

Aunque la interpolación de cadenas es una característica poderosa, es importante ser consciente de los contextos en los que se usa. Por ejemplo, incrustar directamente la entrada del usuario sin una adecuada sanitización puede llevar a problemas de seguridad, tales como ataques de inyección. Los desarrolladores de Google Apps Script deben asegurarse de que cualquier contenido dinámico interpolado en las cadenas esté adecuadamente revisado o saneado.

En comparación con otros lenguajes de programación, el concepto de interpolación de cadenas existe ampliamente, con sintaxis variables. Python utiliza f-strings o el método `format`, Ruby utiliza `#{}` dentro de cadenas con comillas dobles, y muchos lenguajes modernos han adoptado características similares debido a la legibilidad y conveniencia que ofrecen.

Aunque Google Apps Script no ofrece características adicionales de interpolación más allá de las proporcionadas por los estándares de ECMAScript, la funcionalidad presente es poderosa y suficiente para la mayoría de los casos de uso. Los desarrolladores que vienen de lenguajes con mecanismos de interpolación más elaborados pueden necesitar ajustar sus expectativas, pero probablemente apreciarán la simplicidad y eficiencia de los literales de plantilla en Google Apps Script.
