---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:37.544557-07:00
description: "Refactorizar, en el l\xE9xico de programaci\xF3n, se refiere al proceso\
  \ de reestructurar el c\xF3digo de computadora existente\u2014cambiando la facturaci\xF3\
  n sin cambiar\u2026"
lastmod: '2024-03-13T22:44:58.543532-06:00'
model: gpt-4-0125-preview
summary: "Refactorizar, en el l\xE9xico de programaci\xF3n, se refiere al proceso\
  \ de reestructurar el c\xF3digo de computadora existente\u2014cambiando la facturaci\xF3\
  n sin cambiar\u2026"
title: "Refactorizaci\xF3n"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Refactorizar, en el léxico de programación, se refiere al proceso de reestructurar el código de computadora existente—cambiando la facturación sin cambiar su comportamiento externo—para mejorar atributos no funcionales. Es un paso vital para los programadores para mejorar la legibilidad del código, reducir la complejidad y potencialmente desenterrar errores latentes, fomentando un mantenimiento más fácil y la escalabilidad futura del código.

## Cómo hacerlo:

En Google Apps Script, un escenario común que se beneficia de la refactorización es la simplificación de scripts engorrosos que interactúan con Google Sheets o Docs. Inicialmente, los scripts pueden estar escritos de una manera rápida y poco elegante para obtener resultados rápidamente. Con el tiempo, a medida que el script crece, se vuelve difícil de manejar. Vamos a recorrer un ejemplo de refactorización para mejorar la legibilidad y eficiencia.

**Script Original:**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

Esta función registra el nombre de cada hoja en una hoja de cálculo de Google. Aunque funciona bien, emplea prácticas de JavaScript obsoletas y carece de claridad.

**Script Refactorizado:**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

En la versión refactorizada, hemos cambiado a usar `const` para variables que no cambian, haciendo nuestra intención más clara. También hemos utilizado el método `forEach`, un enfoque más moderno y conciso para iterar sobre arreglos, mejorando la legibilidad.

**Salida de muestra (para ambos scripts):**

La salida en Logger se verá algo así, asumiendo que su documento de Google Sheets tiene dos hojas llamadas "Gastos" y "Ingresos":

```
[20-04-2023 10:00:00: INFO] Gastos
[20-04-2023 10:00:01: INFO] Ingresos
```

El script refactorizado logra el mismo resultado pero es más limpio y fácil de entender a simple vista.

## Profundización

La refactorización en Google Apps Script parcialmente hereda sus principios de la práctica más amplia de ingeniería de software. Se hizo más reconocido y estructurado como concepto a finales de los años 90, notablemente debido al libro seminal de Martin Fowler "Refactoring: Improving the Design of Existing Code" (1999), que proporcionó una guía completa de varias técnicas de refactorización. Aunque los detalles específicos de la refactorización pueden variar entre lenguajes de programación debido a sus diferencias sintácticas y funcionales, el objetivo central sigue siendo el mismo: mejorar el código sin alterar su comportamiento externo.

En el contexto de Google Apps Script, un aspecto clave a considerar durante la refactorización son las cuotas de servicio y limitaciones impuestas por Google. Un código refactorizado de manera eficiente no solo se lee mejor, sino que también se ejecuta más rápido y de manera más confiable dentro de estas restricciones. Por ejemplo, operaciones por lotes (`Range.setValues()` en lugar de establecer valores una celda a la vez) pueden reducir significativamente el tiempo de ejecución y el consumo de cuota.

Es importante señalar, sin embargo, que para ciertos proyectos complejos, Google Apps Script podría quedarse corto debido a estas mismas limitaciones. En tales casos, buscar alternativas como Google Cloud Functions o el hermano más nuevo de Apps Script, AppSheet, podría ofrecer una mejor escalabilidad y funcionalidad.

En última instancia, aunque la refactorización es una habilidad crítica en el mantenimiento y mejora de proyectos de Google Apps Script, comprender las limitaciones del entorno y considerar soluciones alternativas es igualmente importante para entregar código eficiente, robusto y mantenible.
