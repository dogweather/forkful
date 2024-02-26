---
date: 2024-01-26 03:50:57.351341-07:00
description: "\"Usar un depurador\" se trata de avanzar paso a paso por tu c\xF3digo\
  \ Python para descubrir errores y entender el comportamiento. Lo hacemos porque\
  \ es mucho\u2026"
lastmod: '2024-02-25T18:49:55.182105-07:00'
model: gpt-4-0125-preview
summary: "\"Usar un depurador\" se trata de avanzar paso a paso por tu c\xF3digo Python\
  \ para descubrir errores y entender el comportamiento. Lo hacemos porque es mucho\u2026"
title: Usando un depurador
---

{{< edit_this_page >}}

## Qué y Por Qué
"Usar un depurador" se trata de avanzar paso a paso por tu código Python para descubrir errores y entender el comportamiento. Lo hacemos porque es mucho más fácil que simplemente adivinar dónde salieron mal las cosas, y nos ahorra horas en el purgatorio de las declaraciones de impresión.

## Cómo hacerlo:
Desglosemos el uso de `pdb`, el depurador integrado de Python. Imagina un archivo, `buggy.py`, con un error difícil de encontrar:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

Al ejecutar este script, esperas `8`, pero solo arroja un error de sintaxis. ¡Es hora del depurador!

En tu terminal, ejecuta:
```bash
python -m pdb buggy.py
```

Entrarás al depurador, y se verá así:
```Python
> /ruta_al_archivo/buggy.py(1)<module>()
-> def add_one(number):
```

Usa `l(ist)` para ver más código, `n(ext)` para ir a la siguiente línea, o `c(ontinue)` para seguir ejecutando el script. Cuando encuentres el error, `pdb` se detendrá y te permitirá inspeccionar.

Después de corregir `number ++ 1` a `number + 1`, reinicia el depurador para probar la corrección.
Recuerda, los amigos no dejan que los amigos codifiquen sin red. 'Nuff said.

## Inmersión Profunda
En la Edad Oscura de la programación (también conocida como antes de que los entornos de desarrollo integrados, o IDEs, estuvieran en todas partes), los depuradores a menudo eran herramientas independientes que usarías fuera de tu editor de texto. Venían al rescate permitiendo a los programadores inspeccionar el estado de su software en varios puntos de ejecución.

A partir de 2023, el `pdb` de Python no es el único juego en la ciudad. La gente podría usar IDEs como PyCharm o Visual Studio Code, que tienen sus propios depuradores integrados. Estos añaden características útiles como puntos de interrupción que puedes establecer con un clic, en lugar de escribir comandos crípticos.

Luego está `ipdb`, un paquete instalable con pip que trae la bondad de `IPython` a la depuración. Es como `pdb` en esteroides, con completado de tabulador y resaltado de sintaxis.

Los depuradores también varían en su implementación. Algunos se acercan mucho a la ejecución del programa a nivel de código de máquina o bytecode. Otros, como muchos depuradores de lenguajes de alto nivel, ejecutan el código en un entorno especial que monitorea los estados de las variables y controla el flujo de ejecución.

## Ver También
Para obtener toda la información sobre el propio depurador de Python, consulta:
- La documentación de `pdb`: https://docs.python.org/3/library/pdb.html

Si tienes curiosidad sobre alternativas, estos enlaces te serán útiles:
- Repositorio y guía de uso de `ipdb`: https://github.com/gotcha/ipdb
- Depurando con Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- Características de depuración de PyCharm: https://www.jetbrains.com/help/pycharm/debugging-code.html

¡Feliz caza de errores!
