---
title:                "Usando expresiones regulares"
aliases: - /es/typescript/using-regular-expressions.md
date:                  2024-02-03T19:18:31.146428-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando expresiones regulares"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Las expresiones regulares, o regex, son una herramienta poderosa para coincidencia de patrones y búsqueda en programación. Los programadores utilizan regex para tareas como validar entradas de usuarios, buscar en texto o manipular cadenas de caracteres porque es eficiente y versátil.

## Cómo hacerlo:

Saltemos a TypeScript y veamos cómo se utiliza regex para tareas comunes.

```TypeScript
// Definir un patrón regex para una dirección de correo electrónico
const emailPattern = /\S+@\S+\.\S+/;

// Probar si una cadena coincide con el patrón de correo electrónico
const email = "usuario@example.com";
console.log(emailPattern.test(email)); // Salida: true

// Encontrar y reemplazar dígitos en una cadena
const replaceDigits = "Artículo 25 cuesta $30".replace(/\d+/g, '#');
console.log(replaceDigits); // Salida: "Artículo # cuesta $#"

// Extraer partes específicas de una cadena usando grupos de captura
const data = "Abril 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, mes, día, año] = datePattern.exec(data) || [];
console.log(mes, día, año); // Salida: "Abril" "10" "2021"
```

## Profundización

Atrás en los años 50, el matemático Stephen Kleene describió las expresiones regulares como un modelo para representar lenguajes regulares, que más tarde se volvieron esenciales en ciencia de la computación. Avanzando rápido, el regex es ubicuo en programación para tratar con texto.

Aunque regex es una navaja suiza para operaciones con cadenas, no está sin alternativas. Dependiendo de la complejidad de la tarea, a veces métodos de cadena como `includes()`, `startsWith()`, `endsWith()`, o incluso análisis con una biblioteca pueden ser mejores. Por ejemplo, analizar una cadena JSON compleja usando regex puede ser una pesadilla—usa un analizador JSON en su lugar.

En cuanto a la implementación, regex en JavaScript y TypeScript se basa en la especificación del lenguaje ECMAScript. Por debajo del capó, los motores utilizan máquinas de estados para coincidir patrones eficientemente. Vale la pena notar que las operaciones regex pueden ser costosas en términos de rendimiento, especialmente con patrones mal escritos—ten cuidado con el "backtracking catastrófico".

## Consulta También

- Documentos Web MDN sobre Expresiones Regulares: [Expresiones Regulares MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101: una herramienta para probar y depurar patrones regex [Regex101](https://regex101.com/)
- Libro "Dominando las Expresiones Regulares" para una comprensión profunda: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
