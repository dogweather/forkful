---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Comenzando un Nuevo Proyecto en TypeScript 

## ¿Qué & Por Qué?
Iniciar un nuevo proyecto significa comenzar a trabajar desde cero, en un código completamente nuevo. Los programadores lo hacen para crear soluciones a medida, y para tener todo controlado en términos de estructuras y dependencias.

## ¿Cómo?
Vamos a crear un nuevo proyecto simple en TypeScript.

Primero, necesitarás instalar TypeScript globalmente en tu máquina:

```TypeScript
npm install -g typescript
```

Luego, crea un nuevo directorio y accede a él:

```TypeScript
mkdir nuevoProyecto
cd nuevoProyecto
```

A continuación, inicializa un nuevo proyecto de Node.js:

```TypeScript
npm init -y
```

Después, agrega TypeScript como dependencia de desarrollo:

```TypeScript
npm i -D typescript
```

Finalmente, puedes hacer `tsc --init` para inicializar un proyecto TypeScript y generar un archivo `tsconfig.json`.

## Profundización
Iniciar un nuevo proyecto en TypeScript tiene sus raíces en los fundamentos de la programación, esencialmente se trata de comenzar con una pizarra en blanco y verla transformarse en algo funcional y útil.

Una alternativa sería trabajar en un proyecto existente, pero eso podría tener el inconveniente de tratar con el código base de otra persona. Aunque puede ser útil para la continuidad del trabajo, a veces puede ser un dolor de cabeza entender y manejar las estructuras y abstracciones implementadas por otros.

Además, cuando hablamos de detalles de implementación, TypeScript introduce la idea de tener archivos fuertemente tipados. Esto significa que cada vez que creas un archivo .ts, tienes que especificar los tipos de tus variables, argumentos y retornos.

## Ver También
Si estás interesado en aprender más sobre TypeScript, te recomiendo los siguientes recursos:

1. [Documentación Oficial de TypeScript](https://www.typescriptlang.org/docs/) - Este es el recurso definitivo para aprender TypeScript directamente de la fuente.
2. [TypeScript Deep Dive](https://basarat.gitbook.io/typescript/) - Un libro que cubre TypeScript de manera exhaustiva.
4. [Curso de TypeScript en Codecademy](https://www.codecademy.com/learn/learn-typescript) - Otro gran recurso para aprender TypeScript de manera efectiva.