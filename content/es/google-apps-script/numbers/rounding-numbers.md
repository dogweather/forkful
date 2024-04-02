---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:29.172102-07:00
description: "Redondear n\xFAmeros, un concepto fundamental en la programaci\xF3n\
  \ de computadoras, involucra ajustar un n\xFAmero a su entero m\xE1s cercano o a\
  \ un n\xFAmero\u2026"
lastmod: '2024-03-13T22:44:58.519593-06:00'
model: gpt-4-0125-preview
summary: "Redondear n\xFAmeros, un concepto fundamental en la programaci\xF3n de computadoras,\
  \ involucra ajustar un n\xFAmero a su entero m\xE1s cercano o a un n\xFAmero\u2026"
title: "Redondeo de n\xFAmeros"
weight: 13
---

## Qué y Por Qué?

Redondear números, un concepto fundamental en la programación de computadoras, involucra ajustar un número a su entero más cercano o a un número especificado de decimales. Los programadores a menudo realizan redondeos para simplificar números para la legibilidad humana o para cumplir con necesidades de cálculo específicas, asegurando precisión y reduciendo la carga computacional.

## Cómo:

Google Apps Script, al ser un lenguaje basado en JavaScript, ofrece métodos estándar para redondear números. Aquí hay una desglose de tres técnicas comúnmente usadas:

### Math.round()
Esta función redondea un número al entero más cercano.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // Salidas: 3
```

### Math.ceil()
Redondea un número al entero más cercano hacia arriba.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // Salidas: 3
```

### Math.floor()
En contraste, redondea un número al entero más cercano hacia abajo.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // Salidas: 2
```

Para lugares decimales específicos, puedes usar `.toFixed()`, que de hecho devuelve una cadena, o un enfoque más matizado para el redondeo matemático:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // Salidas: "2.57" (como una cadena)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // Salidas: 2.57
```

## Inmersión Profunda

Redondear números en Google Apps Script no se desvía mucho de cómo se hace en otros entornos de JavaScript. Sin embargo, entender las diferencias en los métodos de redondeo y el potencial para problemas de aritmética de punto flotante es crucial. Por ejemplo, debido a la forma en que las computadoras representan los flotantes, no todas las fracciones decimales pueden representarse con precisión perfecta, llevando a veces a resultados de redondeo inesperados.

Históricamente, JavaScript (y por extensión, Google Apps Script) maneja esto al conformarse con el estándar IEEE 754, utilizado por muchos otros lenguajes de programación para la aritmética de punto flotante. Este estándar define cómo se redondean los números, asegurando consistencia a través de varias plataformas y lenguajes.

Si bien los métodos de redondeo directo en Google Apps Script son sencillos y a menudo suficientes, aplicaciones complejas o de alta precisión podrían beneficiarse de bibliotecas como decimal.js o big.js, que están diseñadas para manejar aritmética de precisión arbitraria. Estas pueden ser especialmente útiles cuando se trabaja con cálculos financieros o científicos donde la precisión de los números redondeados es primordial.

Recuerda, sin embargo, que aprovechar las bibliotecas externas en Google Apps Script requiere cargarlas a través del editor de scripts, lo que puede introducir dependencias o afectar el rendimiento de tu script dependiendo de cómo se utilice. En muchos casos, los métodos Math integrados son completamente adecuados, pero para esos casos límite que requieren precisión al grado nth, mirar más allá de la biblioteca estándar puede ser necesario.
