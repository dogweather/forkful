---
date: 2024-01-26 04:10:58.945322-07:00
description: "Para empezar con un depurador en TypeScript, todo lo que necesita es\
  \ un IDE compatible (como Visual Studio Code) y una configuraci\xF3n `launch.json`.\
  \ Aqu\xED\u2026"
lastmod: '2024-03-13T22:44:58.805760-06:00'
model: gpt-4-0125-preview
summary: "Para empezar con un depurador en TypeScript, todo lo que necesita es un\
  \ IDE compatible (como Visual Studio Code) y una configuraci\xF3n `launch.json`.\
  \ Aqu\xED\u2026"
title: Usando un depurador
weight: 35
---

## Cómo hacerlo:
Para empezar con un depurador en TypeScript, todo lo que necesita es un IDE compatible (como Visual Studio Code) y una configuración `launch.json`. Aquí tienes un ejemplo rápido para una aplicación Node.js:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hola, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

Para depurar esto, crea un archivo `launch.json` bajo la carpeta `.vscode`:

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Iniciar Programa",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

Luego, establece un punto de interrupción en tu función `greet` haciendo clic en el lado izquierdo del número de línea en tu IDE. Presiona F5 para comenzar a depurar y observa cómo tu aplicación se pausa en el punto de interrupción. Ahora puedes pasar el mouse sobre las variables, observar expresiones y avanzar paso a paso por tu código con facilidad.

## Inmersión Profunda
En los viejos tiempos, antes de que los entornos de desarrollo integrados (IDEs) se volvieran elegantes, la depuración a menudo se hacía con declaraciones de impresión (conocido también como depuración `console.log`). Funcionaba, más o menos, pero era como tratar de encontrar una aguja en un pajar con los ojos vendados.

Los depuradores modernos son como una navaja suiza para la resolución de problemas. Con la evolución de TypeScript y Node.js, hay varios depuradores disponibles, desde el inspector Node.js incorporado hasta las herramientas de desarrollo del navegador para la depuración del lado del cliente.

El inspector de Node.js funciona adjuntándose a su aplicación en ejecución; se comunica a través del Protocolo de Herramientas para Desarrolladores de Chrome, convirtiendo su navegador Chrome en una consola de depuración poderosa. Esta integración permite una sesión de depuración visualmente interactiva y detallada, en comparación con las prácticas tradicionales de depuración de línea de comandos.

## Ver También
Para una lectura adicional y algunos consejos de profesionales, consulta:

- [Depuración de TypeScript en Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Guía de Depuración de Node.js](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Documentación de Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
