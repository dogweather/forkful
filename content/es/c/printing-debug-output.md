---
title:                "C: Imprimiendo salida de depuración"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué imprimir salida de depuración en tu programa C? 

Imprimir salida de depuración es una práctica común en la programación en C. Esta técnica es útil para ayudar a los desarrolladores a encontrar errores o problemas en su código. Al imprimir información en diferentes puntos de tu programa, puedes rastrear dónde ocurren los errores y solucionarlos más fácilmente.

## Cómo hacerlo

Para imprimir salida de depuración en tu programa C, puedes usar la función `printf` en la biblioteca estándar de C. Esta función toma una cadena de formato como primer argumento y cualquier número de argumentos adicionales que se imprimen de acuerdo al formato especificado en la cadena.

```C
printf("El valor de mi_variable es: %d", mi_variable);
```

En el ejemplo anterior, la cadena de formato "%d" indica que se imprimirá un número decimal y `mi_variable` es el valor que se imprimirá en su lugar.

Otra forma útil de imprimir salida de depuración es usando la macro `assert` en la biblioteca `assert.h`. Esta macro evalúa una condición y, si es falsa, imprime un mensaje de error y termina la ejecución del programa.

```C
int x = 5;
assert(x > 10);
```

En este ejemplo, como la condición `x > 10` es falsa, se imprimirá un mensaje de error que te ayudará a encontrar rápidamente el problema en tu código.

## Profundizando en la impresión de salida de depuración

Además de simplemente imprimir valores de variables, también puedes incluir mensajes descriptivos en tu salida de depuración. Esto puede ser útil cuando tienes múltiples puntos de impresión en tu programa y quieres saber exactamente dónde se está imprimiendo cada valor.

También puedes imprimir información sobre el valor de punteros o estructuras de datos en tu programa. Simplemente asegúrate de indicar el formato adecuado en la cadena de formato `printf` o `assert`.

En general, imprimir salida de depuración es una herramienta poderosa para detectar y solucionar errores en tus programas C. Sin embargo, asegúrate de eliminar todas las impresiones de depuración antes de liberar tu programa a producción, ya que pueden afectar negativamente al rendimiento.

## Ver también

- [Depuración en C - Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_debugging.htm)
- [Funciones de la biblioteca estándar de C - The GNU C Library](https://www.gnu.org/software/libc/manual/html_node/Standard-Functions.html)
- [Macros assert en C - TechOnTheNet](https://www.techonthenet.com/c_language/standard_library_functions/assert_h/assert_h_macro.php)