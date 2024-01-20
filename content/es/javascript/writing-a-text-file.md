---
title:                "Escritura de un archivo de texto"
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?

Escribir un archivo de texto en Javascript te permite guardar datos en tu disco local. Los programadores hacen esto para preservar información entre sesiones, compartir datos con otros programas o simplemente para almacenar outputs de una forma permanente.

## Cómo hacerlo:

Para escribir archivos de texto en JavaScript del lado del servidor, utilizamos Node.js. Aquí te muestro cómo:

```javascript
const fs = require('fs');

let data = "Este es el texto que quiero guardar en un archivo.";

fs.writeFile('archivo.txt', data, (err) => {
    if (err) throw err;
    console.log('El archivo ha sido guardado!');
});
```

Si corres este script en Node.js, crearás un archivo llamado `archivo.txt` con el texto especificado dentro. Verás la siguiente salida en la consola:

```
El archivo ha sido guardado!
```

Para el lado del cliente, la cosa cambia un poco. Los navegadores restringen la escritura directa en el sistema de archivos del usuario por seguridad. Aun así, puedes crear y descargar archivos así:

```javascript
let data = "Este es el texto que quiero guardar en un archivo.";
let filename = 'archivo.txt';
let type = 'text/plain';

let file = new Blob([data], {type: type});

let a = document.createElement("a"),
    url = URL.createObjectURL(file);
a.href = url;
a.download = filename;
document.body.appendChild(a);
a.click();

setTimeout(function() {
    document.body.removeChild(a);
    window.URL.revokeObjectURL(url);  
}, 0);
```

Este código generará un archivo `archivo.txt` que el usuario puede descargar en su máquina.

## Deep Dive

Antes de Node.js, escribir archivos de texto en JavaScript no era tarea sencilla ya que JavaScript se diseñó principalmente para manipular documentos web y no directamente el sistema de archivos. Node.js introdujo un módulo de sistema de archivos (`fs`) que proporciona esta funcionalidad.

Existen alternativas como las bases de datos o almacenamiento en la nube, pero la escritura en archivos locales es útil para tareas de scripting y automatización.

Detalles de implementación: para evitar problemas con operaciones asíncronas, puedes usar `fs.writeFileSync()` para una versión síncrona, pero bloquearás el hilo de ejecución durante la escritura, lo cual no es recomendable en entornos de producción.

## See Also

Aquí te dejo unos enlaces para que profundices más:

- Documentación de Node.js [`fs`](https://nodejs.org/api/fs.html) module, para entender todas las funciones disponibles para trabajar con el sistema de archivos.
- MDN Web API [`Blob`](https://developer.mozilla.org/en-US/docs/Web/API/Blob) para más información sobre cómo manejar objetos binarios grandes en el navegador.
- [`FileSaver.js`](https://github.com/eligrey/FileSaver.js/) para una solución más sofisticada de guardar archivos desde el navegador.