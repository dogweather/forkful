---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:07.836157-07:00
description: "Una shell interactiva, o Bucle de Leer-Evaluar-Imprimir (REPL, por sus\
  \ siglas en ingl\xE9s), te permite experimentar con c\xF3digo de Go en tiempo real,\u2026"
lastmod: '2024-03-13T22:44:58.469249-06:00'
model: gpt-4-0125-preview
summary: "Una shell interactiva, o Bucle de Leer-Evaluar-Imprimir (REPL, por sus siglas\
  \ en ingl\xE9s), te permite experimentar con c\xF3digo de Go en tiempo real, ejecutando\
  \ comandos y obteniendo retroalimentaci\xF3n inmediata."
title: Utilizando un shell interactivo (REPL)
weight: 34
---

## Cómo hacerlo:
Aunque Go no incluye un REPL incorporado, la comunidad ha creado herramientas como `gore` para llenar ese vacío. Primero, instala `gore` ejecutando:

```
$ go get -u github.com/motemen/gore
```

Una vez instalado, inicia `gore` escribiendo `gore` en tu terminal:

```
$ gore
```

Deberías ver un prompt listo para aceptar comandos de Go. Probemos un ejemplo simple:

```
gore> :import fmt
gore> fmt.Println("¡Hola, Go REPL!")
```

Verías una salida como:

```
¡Hola, Go REPL!
```

Las variables y definiciones de funciones funcionan como se espera. Puedes declarar una función:

```
gore> :import math
gore> areaCirculo := func(radio float64) float64 {
...> return math.Pi * radio * radio
...> }
gore> fmt.Println("Área de un círculo con radio 4:", areaCirculo(4))
```

Y obtener la salida de inmediato:

```
Área de un círculo con radio 4: 50.26548245743669
```

## Inmersión Profunda:
El concepto de un REPL es antiguo, remontándose a las máquinas Lisp de los años 60, proporcionando una experiencia de programación interactiva. A diferencia de lenguajes como Python o JavaScript, Go fue diseñado sin un REPL, enfocándose en su lugar en binarios compilados para el rendimiento y simplicidad. Esto refleja la filosofía de Go de simplicidad y su diseño para software escalable y mantenible.

Sin embargo, herramientas como `gore` o `goplay` muestran la ingeniosidad de la comunidad de Go en cerrar esta brecha. Estas herramientas parsean código de Go dinámicamente y usan el paquete `go/eval` o mecanismos similares para ejecutarlo en tiempo real, aunque con algunas limitaciones en comparación con un ambiente REPL nativo. Estas limitaciones se derivan del sistema de tipos de Go y el modelo de compilación, lo que puede hacer que la evaluación al vuelo sea un desafío.

Aunque los ambientes REPL son excepcionalmente útiles para la educación y pruebas rápidas, el ecosistema de Go típicamente gravita hacia procesos tradicionales de compilar y ejecutar para la mayoría de las tareas de desarrollo. IDEs y editores con soporte para Go, como Visual Studio Code o GoLand, ofrecen herramientas integradas para pruebas y depuración que alivian mucha de la necesidad de un REPL para el desarrollo profesional.

Para programación exploratoria, prototipado, o aprendizaje, sin embargo, REPLs como `gore` ofrecen una alternativa valiosa, permitiendo a los programadores acostumbrados a REPLs en otros lenguajes disfrutar de una experiencia similar en Go.
