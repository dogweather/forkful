---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:06.147087-07:00
description: "Un shell interactivo, tambi\xE9n conocido como un bucle Leer-Evaluar-Imprimir\
  \ (REPL, por sus siglas en ingl\xE9s), permite a los programadores escribir\u2026"
lastmod: '2024-02-25T18:49:56.015793-07:00'
model: gpt-4-0125-preview
summary: "Un shell interactivo, tambi\xE9n conocido como un bucle Leer-Evaluar-Imprimir\
  \ (REPL, por sus siglas en ingl\xE9s), permite a los programadores escribir\u2026"
title: Utilizando un shell interactivo (REPL)
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Un shell interactivo, también conocido como un bucle Leer-Evaluar-Imprimir (REPL, por sus siglas en inglés), permite a los programadores escribir expresiones o código y ver inmediatamente los resultados, mejorando los procesos de aprendizaje y depuración. A pesar de que C tradicionalmente no soporta entornos REPL de forma nativa, las herramientas modernas cierran esta brecha, ofreciendo una exploración dinámica de los programas en C.

## Cómo hacerlo:

Para interactuar con un REPL en C, es posible que no encuentres un camino tan directo como en lenguajes como Python o JavaScript. Sin embargo, herramientas como `Cling`, un intérprete de C/C++ basado en la tecnología Clang y LLVM, hacen posible esta tarea. Aquí te explicamos cómo empezar:

1. **Instalar Cling**: Dependiendo de tu sistema operativo, podrías encontrar Cling en tu gestor de paquetes o necesitar construirlo desde el código fuente. Por ejemplo, en Ubuntu, podría ser tan simple como `sudo apt-get install cling`.

2. **Iniciar Cling**: Abre tu terminal y escribe `cling` para comenzar el shell interactivo.

```bash
$ cling
```

3. **Escribir Código**: Ahora puedes escribir código en C directamente en el shell y ver resultados inmediatos. Aquí tienes un ejemplo simple:

```c
[cling]$ #include <stdio.h>
[cling]$ printf("¡Hola, mundo REPL!\n");
¡Hola, mundo REPL!
```

4. **Ejemplo con Variables y Operaciones**: Experimenta con variables y ve feedback instantáneo.

```c
[cling]$ int a = 5;
[cling]$ int b = 3;
[cling]$ printf("%d + %d = %d\n", a, b, a+b);
5 + 3 = 8
```

5. **Incluir Librerías**: Cling te permite incluir librerías sobre la marcha, habilitando así una amplia gama de funcionalidades de C.

```c
[cling]$ #include <math.h>
[cling]$ printf("La raíz cuadrada de %f es %f\n", 4.0, sqrt(4.0));
La raíz cuadrada de 4.000000 es 2.000000
```

## Profundización:

El inicio de los entornos REPL se remonta a Lisp en la década de 1960, diseñados para soportar la evaluación de código interactiva. Sin embargo, la naturaleza estática y compilada de C presentaba desafíos para realizar ajustes inmediatos en la ejecución del código. El desarrollo de Cling y otros intérpretes de C/C++ marca avances significativos hacia la integración de la evaluación dinámica en los lenguajes de tipado estático.

Es notable que el uso de un intérprete como Cling podría no reflejar perfectamente el comportamiento del código C compilado debido a diferencias en la optimización y ejecución. Además, aunque son muy valiosos para fines educativos, prototipos rápidos y depuración, los REPL para C pueden ser a veces más lentos y menos prácticos para el desarrollo de código a nivel de producción en comparación con los ciclos tradicionales de compilar-ejecutar-depurar.

Las alternativas para la programación interactiva en C incluyen escribir programas pequeños y autónomos y usar IDE robustos con herramientas de depuración integradas, que pueden ofrecer más control e información sobre la ejecución, aunque con menos inmediatez. A pesar de estas alternativas, la llegada de entornos REPL en C representa una expansión emocionante de la versatilidad del lenguaje, abrazando las demandas de la era moderna por flexibilidad y velocidad en los ciclos de desarrollo.
