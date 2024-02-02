---
title:                "Escribiendo pruebas"
date:                  2024-02-01T22:09:34.645246-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo pruebas"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/google-apps-script/writing-tests.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir pruebas en Google Apps Script (GAS) se trata de crear scripts automatizados para verificar el comportamiento de tus códigos, asegurando que se desempeñen como se espera bajo diversas condiciones. Los programadores lo hacen para detectar errores temprano, mejorar la calidad del código y facilitar actualizaciones y mantenimiento más fáciles.

## Cómo:

Aunque Google Apps Script no tiene un marco de pruebas integrado como algunos otros entornos de programación, todavía puedes escribir y ejecutar pruebas aprovechando funciones simples de GAS o integrando bibliotecas de pruebas externas como `QUnit`. Aquí hay un ejemplo básico usando una función simple de GAS para probar otra función en tu script:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Prueba fallida: add(2, 3) debería ser 5, pero fue " + result);
  } else {
    Logger.log("¡Prueba exitosa!");
  }
}
```

Ejecutar `testAdd()` registrará "¡Prueba exitosa!" si la función `add` funciona correctamente, o lanzará un error si no lo hace. Para un enfoque más sofisticado, integrar QUnit con Google Apps Script involucra algunos pasos más pero ofrece un entorno de pruebas poderoso. Una configuración de prueba de muestra con QUnit se ve así:

1. Incluir la biblioteca QUnit en tu proyecto.
2. Crear un archivo HTML de prueba para ejecutar las pruebas de QUnit.
3. Escribir casos de prueba usando la sintaxis de QUnit.

Aquí hay un ejemplo usando QUnit:

```javascript
// Incluye QUnit enlazándolo en un archivo HTML utilizado para ejecutar tus pruebas

QUnit.test("Probando la función add", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) debería devolver 5");
});
```

Para ver los resultados, abre el archivo HTML dentro del Editor de Scripts de GAS o despliégalo como una aplicación web.

## Estudio a Fondo

Históricamente, las pruebas en Google Apps Script han sido algo pasadas por alto, probablemente debido a los orígenes de la plataforma y los casos de uso primarios enfocados en tareas de automatización rápidas y a pequeña escala en lugar de aplicaciones grandes. Como tal, GAS no ofrece los mismos marcos y herramientas de pruebas robustas encontradas en entornos de programación más tradicionales. Sin embargo, la comunidad se ha adaptado incorporando bibliotecas de código abierto y aprovechando de manera creativa las herramientas existentes de Google.

Usar bibliotecas como QUnit representa un paso importante hacia adelante, pero viene con su propio conjunto de desafíos, como configurar un entorno de pruebas adecuado y aprender una sintaxis adicional. Sin embargo, para aquellos invertidos en construir aplicaciones más complejas y confiables con GAS, el esfuerzo vale la pena.

Alternativas como usar funciones simples de GAS para pruebas ofrecen facilidad de uso e integración con el entorno de GAS sin dependencias adicionales, pero carecen de características de pruebas exhaustivas y la capacidad de escalar fácilmente a medida que tu proyecto crece. Herramientas como clasp (la Interfaz de Línea de Comandos de Google Apps Script) pueden facilitar flujos de trabajo más avanzados, incluidas las pruebas, al permitir que los desarrolladores codifiquen en su IDE preferido, introduciendo espacio para integrarse más fácilmente con marcos de pruebas externos.

En conclusión, aunque GAS tal vez no tenga soporte nativo para pruebas sofisticadas de inmediato, su flexibilidad y los enfoques innovadores de la comunidad brindan vías viables para asegurar que tus scripts sean robustos, confiables y listos para cualquier tarea.
