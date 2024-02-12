---
title:                "Generación de números aleatorios"
date:                  2024-01-27T20:35:34.836982-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generación de números aleatorios"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Generar números aleatorios en TypeScript es acerca de crear valores numéricos impredecibles dentro de un rango especificado. Los programadores aprovechan estos dígitos aleatorios para una variedad de propósitos, tales como generar identificadores únicos, simular datos para pruebas o añadir imprevisibilidad a juegos y simulaciones.

## Cómo hacerlo:

En TypeScript, puedes generar números aleatorios utilizando el objeto global `Math`. A continuación se presentan algunos ejemplos prácticos que demuestran cómo producir números aleatorios para diferentes requisitos.

### Generando un Número Aleatorio Básico

Para generar un número decimal aleatorio básico entre 0 (inclusive) y 1 (exclusivo), usas `Math.random()`. Esto no requiere ninguna manipulación adicional:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

Esto podría producir un valor como `0.8995452185604771`.

### Generando un Entero Aleatorio Entre Dos Valores

Cuando necesitas un entero entre dos valores específicos, incorporas tanto `Math.random()` como algo de aritmética:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

Esto podría producir un valor entero entre 1 y 10, como `7`.

### Generando un Identificador Único

Los números aleatorios pueden combinarse con otros métodos para crear identificadores únicos, por ejemplo, un fragmento de código para generar un UUID simple:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

Esto genera una cadena que se asemeja a un UUID, como `110e8400-e29b-41d4-a716-446655440000`.

## Investigación Profunda

El método principal para generar números aleatorios en JavaScript y, por ende, en TypeScript, `Math.random()`, depende de un generador de números aleatorios pseudoaleatorios (PRNG). Es importante notar que mientras los resultados puedan parecer aleatorios, son generados por un algoritmo determinista basado en un valor semilla inicial. Por lo tanto, los números producidos por `Math.random()` no son verdaderamente aleatorios y no deben usarse para propósitos criptográficos.

Para números aleatorios criptográficamente seguros, la Web Crypto API ofrece `crypto.getRandomValues()`, que es accesible en entornos que soportan el estándar Web Crypto, incluyendo navegadores modernos y Node.js (a través del módulo `crypto`). Aquí hay un ejemplo rápido que ilustra su uso en TypeScript para generar un número aleatorio seguro dentro de un rango:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

Este método proporciona un nivel más fuerte de aleatoriedad y es más adecuado para aplicaciones sensibles a la seguridad. Sin embargo, también es más intensivo en recursos y puede no ser necesario para tareas más mundanas, como simulaciones simples o la generación de valores aleatorios no críticos.
