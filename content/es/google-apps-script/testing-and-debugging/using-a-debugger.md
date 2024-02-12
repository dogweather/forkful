---
title:                "Usando un depurador"
aliases:
- es/google-apps-script/using-a-debugger.md
date:                  2024-02-01T22:03:03.581800-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/google-apps-script/using-a-debugger.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Depurar en Google Apps Script (GAS) implica el proceso de identificación y eliminación de errores de los scripts destinados a automatizar Google Apps o construir aplicaciones web. Los programadores depuran para asegurar que su código se ejecute como se espera, mejorando la fiabilidad y rendimiento en las aplicaciones.

## Cómo:

Google Apps Script proporciona un depurador integrado dentro del Editor de Apps Script para ayudar a solucionar problemas en los scripts. Así es como se inicia y se usa el depurador:

1. **Abre tu script en el Editor de Apps Script.**
2. **Selecciona una función para depurar.** Desde el menú desplegable en la parte superior, selecciona la función que deseas depurar.
3. **Establece puntos de interrupción.** Haz clic en el margen (el área gris a la izquierda de los números de línea) donde deseas pausar la ejecución; aparecerá un punto rojo, indicando un punto de interrupción.
4. **Inicia la depuración.** Haz clic en el icono de bug o selecciona `Depurar` > `Iniciar depuración`. La ejecución iniciará y se pausará en el primer punto de interrupción.

Considera este script simple:

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // Se pretende registrar 15
}
```

Si no estás seguro de por qué `Logger.log(sum)` no está mostrando el resultado esperado, podrías establecer un punto de interrupción en la línea `var sum = a + b;` y recorrer el script línea por línea para inspeccionar los valores de las variables.

**Salida de muestra en Logger:**

```plain
15
```

Mientras depuras, el Editor de Apps Script te permite:

- **Avanzar por el código** usando los botones de avanzar, entrar y salir.
- **Observar expresiones y variables** para ver cómo cambian sus valores en tiempo real.
- **Inspeccionar la pila de llamadas** para rastrear las llamadas a funciones.

## Análisis Profundo

Depurar en Google Apps Script, como en cualquier otro entorno de programación, es esencial para crear aplicaciones libres de errores. Introducido al principio del desarrollo de GAS, el depurador integrado ofrece capacidades fundamentales para inspeccionar y corregir el código de manera incremental. Aunque proporciona características de depuración básicas similares a las encontradas en entornos más maduros como Visual Studio Code o IntelliJ, puede quedarse corto para escenarios de depuración complejos. Por ejemplo, sus capacidades para inspeccionar llamadas asincrónicas o manejar ejecuciones de scripts pesados podrían ser limitantes.

Para necesidades de depuración complejas, los desarrolladores podrían recurrir a métodos alternativos como la creación extensiva de registros (usando `Logger.log()`) o incluso desplegando como una aplicación web para inspeccionar el comportamiento en un escenario real. Sin embargo, la simplicidad e integración del depurador de GAS dentro del Editor de Apps Script lo convierte en un primer paso invaluable para la solución de problemas y el entendimiento del comportamiento del script. Notablemente, con las actualizaciones y mejoras continuas de Google a Apps Script, la experiencia de depuración está mejorando constantemente, ofreciendo herramientas y opciones más sofisticadas con el tiempo. Esta evolución refleja el compromiso de Google de hacer de Apps Script una plataforma más poderosa y accesible para desarrolladores de diversos orígenes.
