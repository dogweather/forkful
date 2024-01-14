---
title:    "Arduino: Buscando y reemplazando texto."
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

A veces, cuando estamos escribiendo un programa en Arduino, necesitamos realizar cambios en el código para corregir errores o mejorar la funcionalidad. La búsqueda y reemplazo de texto es una forma rápida y eficiente de realizar estos cambios en todo nuestro código.

## Cómo hacerlo

Para buscar y reemplazar texto en Arduino, podemos utilizar la función `findAndReplace`. Esta función toma tres parámetros: la palabra que estamos buscando, la palabra con la que queremos reemplazarla y el string en el que queremos realizar la búsqueda.

```Arduino
findAndReplace("hola", "adiós", texto);
```

Este código reemplazará cada instancia de "hola" por "adiós" en el string `texto`. Con esto, podemos solucionar rápidamente errores ortográficos o actualizar nombres de variables en todo nuestro código.

## Inmersión Profunda

Aunque la función `findAndReplace` es muy útil para cambios simples en nuestro código, debemos tener en cuenta algunos detalles importantes. Por ejemplo, esta función solo remplazará la primera instancia de la palabra buscada en cada línea de texto. Para reemplazar todas las instancias, debemos usar un bucle `while` y llamar a `findAndReplace` dentro de éste.

Otro detalle importante es que la función no reemplaza el texto en sí, sino que crea una nueva instancia con el texto modificado. Por lo tanto, debemos asegurarnos de almacenar este nuevo string en una variable para utilizarlo en lugar del original.

## Ver También

Para más información sobre la función `findAndReplace`, puedes revisar la documentación oficial de Arduino: https://www.arduino.cc/reference/en/language/functions/advanced-io/findandreplace/