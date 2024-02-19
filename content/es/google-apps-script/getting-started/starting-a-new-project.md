---
aliases:
- /es/google-apps-script/starting-a-new-project/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:31.396592-07:00
description: "Iniciar un nuevo proyecto en Google Apps Script (GAS) implica inicializar\
  \ un archivo de script dentro del ecosistema de Google (Google Drive, Docs, Hojas\u2026"
lastmod: 2024-02-18 23:09:09.507732
model: gpt-4-0125-preview
summary: "Iniciar un nuevo proyecto en Google Apps Script (GAS) implica inicializar\
  \ un archivo de script dentro del ecosistema de Google (Google Drive, Docs, Hojas\u2026"
title: Iniciando un nuevo proyecto
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Iniciar un nuevo proyecto en Google Apps Script (GAS) implica inicializar un archivo de script dentro del ecosistema de Google (Google Drive, Docs, Hojas de cálculo, etc.) para automatizar tareas o ampliar las funcionalidades de las Aplicaciones de Google. Los programadores a menudo se embarcan en este viaje para optimizar flujos de trabajo, manipular servicios de Google programáticamente o crear complementos personalizados, ahorrando tiempo y aprovechando el poder de la infraestructura de Google.

## Cómo hacerlo:

Para comenzar un nuevo proyecto en Google Apps Script, tienes un par de puntos de entrada, pero vamos a centrarnos en el método más directo: crear un script desde Google Drive.

1. **Crear un proyecto en Google Drive**
   - Navega a Google Drive (drive.google.com).
   - Haz clic en "+ Nuevo" > "Más" > "Google Apps Script".
   - Un nuevo proyecto de script se abre en el editor. Por defecto, contiene un archivo `Code.gs` con un ejemplo de `myFunction`.

2. **Configurando tu proyecto**
   - Renombra tu proyecto para mayor claridad. Haz clic en "Proyecto sin título" en la parte superior izquierda y dale un nombre significativo.
   - Escribe una función simple en el archivo `Code.gs` para familiarizarte con él:

```javascript
function helloWorld() {
  Logger.log('¡Hola, mundo!');
}
```

   - Ejecuta `helloWorld` seleccionando la función en el menú desplegable junto al botón de reproducción (▶) y haz clic en él. Esto ejecutará la función.

3. **Ver los registros**
   - Para ver la salida de `Logger.log`, ve a "Ver" > "Registros", o presiona `Ctrl + Enter`. Deberías ver "¡Hola, mundo!" en los registros.

¡Felicidades, acabas de empezar con éxito un nuevo proyecto en Google Apps Script y ejecutado una función simple!

## Análisis Profundo

La creación de Google Apps Script alrededor de 2009 proporcionó una plataforma potente pero accesible tanto para desarrolladores como para no desarrolladores para automatizar, ampliar y construir sobre la amplia gama de servicios de Google. A diferencia de los entornos de programación tradicionales, GAS ofrece una mezcla única de simplicidad e integración, directamente dentro del ecosistema de Google, sin necesidad de servidores externos o configuración. Este modelo de ejecución sin servidor simplifica enormemente la implementación y gestión de proyectos.

Históricamente, GAS estaba algo limitado por su entorno de ejecución y versión del lenguaje, a menudo rezagado con respecto a los estándares actuales de JavaScript. Sin embargo, las actualizaciones recientes han traído la sintaxis moderna de JavaScript (ECMAScript 2015+) a GAS, haciéndolo más atractivo para los desarrolladores acostumbrados a las prácticas de desarrollo contemporáneas.

Aunque GAS tiene una posición única para interactuar con los Servicios de Google, hay enfoques alternativos para necesidades más intensivas o específicas. Por ejemplo, Google Cloud Functions y Google Cloud Platform (GCP) ofrecen soluciones más robustas y escalables para manejar flujos de trabajo complejos, procesar grandes conjuntos de datos e integrarse con APIs externas. Estas plataformas permiten la programación en varios lenguajes (por ejemplo, Python, Go, Node.js) y ofrecen mayores recursos computacionales.

Sin embargo, para tareas estrechamente vinculadas a las Aplicaciones de Google, la automatización y el desarrollo rápido dentro de este ecosistema, Google Apps Script sigue siendo una herramienta inigualable en términos de facilidad de uso y profundidad de integración. Su accesibilidad directamente desde Google Drive y la conexión perfecta con los servicios de Google lo hacen una opción práctica para una amplia gama de proyectos, especialmente para aquellos que buscan ampliar la funcionalidad de Hojas de cálculo, Documentos, Formularios y otras aplicaciones de Google.
