---
title:                "Organizando el código en funciones"
aliases: - /es/typescript/organizing-code-into-functions.md
date:                  2024-01-26T01:16:08.012505-07:00
model:                 gpt-4-0125-preview
simple_title:         "Organizando el código en funciones"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Organizar el código en funciones significa dividir tu código en bloques modulares reutilizables. Hacemos esto para mantener las cosas DRY (Don't Repeat Yourself, No Te Repitas), haciendo que el código sea más limpio, fácil de leer y sencillo de depurar.

## Cómo hacerlo:
Imagina que estás haciendo una calculadora básica. En lugar de escribir la lógica de adición en todos los lugares que la necesites, crea una función `add` (añadir):

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // Salida de muestra: 12
```

Ahora, digamos que necesitamos una función para multiplicar:

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // Salida de muestra: 12
```
¿Notas cómo nos centramos en una tarea por función? Ese es el corazón de organizar el código.

## Inmersión Profunda
Históricamente, a medida que los lenguajes de programación evolucionaron, las funciones se convirtieron en vitales para estructurar el código, tomando como referencia las funciones matemáticas. Son un pilar en la programación procedimental y continúan vigentes en los paradigmas de programación orientada a objetos y programación funcional.

¿Alternativas? Podrías simplemente no usar funciones, pero eso es un boleto de ida a la Ciudad Espagueti. O podrías optar por OOP (Programación Orientada a Objetos) y empaquetar la funcionalidad en métodos, que básicamente son funciones que pertenecen a objetos.

En términos de implementación, TypeScript insiste en los tipos. Definir tipos de entrada y salida para las funciones no es solo de buena educación; es un requisito para un código TypeScript limpio. Además, con TypeScript, obtienes características ingeniosas como sobrecargas, genéricos y parámetros opcionales para potenciar tus funciones.

## Ver También
Consulta estos recursos para mejorar tu juego de funciones:

- [TypeScript Handbook – Functions](https://www.typescriptlang.org/docs/handbook/2/functions.html): Tu Biblia para las funciones de TypeScript.
- [Clean Code JavaScript](https://github.com/ryanmcdermott/clean-code-javascript#functions): Aplica principios de Código Limpio a tus funciones de JavaScript.
- [You Don’t Know JS – Scope & Closures](https://github.com/getify/You-Dont-Know-JS): Comprende cómo funcionan las funciones con el alcance y los cierres en JavaScript.
