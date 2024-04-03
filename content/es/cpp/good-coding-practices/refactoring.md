---
date: 2024-01-26 01:17:19.562146-07:00
description: "C\xF3mo hacerlo: Imagina que tienes una funci\xF3n que est\xE1 haciendo\
  \ demasiado, como este m\xE9todo torpe que inicializa un objeto y tambi\xE9n realiza\
  \ registros\u2026"
lastmod: '2024-03-13T22:44:59.383506-06:00'
model: gpt-4-0125-preview
summary: "Imagina que tienes una funci\xF3n que est\xE1 haciendo demasiado, como este\
  \ m\xE9todo torpe que inicializa un objeto y tambi\xE9n realiza registros (logging)."
title: "Refactorizaci\xF3n"
weight: 19
---

## Cómo hacerlo:
Imagina que tienes una función que está haciendo demasiado, como este método torpe que inicializa un objeto y también realiza registros (logging):

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // Lógica de inicialización
        // ...

        // Registro detallado
        if (verbose) {
            std::cout << "¡Widget inicializado!" << std::endl;
        }
    }
};

// Uso:
Widget w;
w.init(true);
```

Salida:
```
¡Widget inicializado!
```

Refactorizar esto en métodos más limpios y enfocados podría verse así:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // Solo lógica de inicialización
        // ...
    }

    void logInitialization() const {
        std::cout << "¡Widget inicializado!" << std::endl;
    }
};

// Uso:
Widget w;
w.init();
w.logInitialization();
```

Este cambio no ha alterado lo que hace el programa pero hace que la clase `Widget` sea más modular y su uso más claro.

## Estudio profundo
El concepto de refactorización como lo conocemos hoy tiene sus raíces en las comunidades de programación de Smalltalk de la década de 1980 y fue fuertemente popularizado por el libro de Martin Fowler "Refactoring: Improving the Design of Existing Code" de 1999. Hoy en día, la refactorización es una parte central del desarrollo de software moderno, integrado en diversas metodologías de desarrollo como Agile y TDD (Desarrollo Dirigido por Pruebas).

Cuando hablamos de alternativas a la refactorización, nos adentramos en el territorio de reescritura o rediseño. La refactorización es estratégica e incremental, mientras que una reescritura puede descartar el código existente en favor de una nueva solución. El rediseño, mientras tanto, puede implicar cambios más significativos, incluyendo alterar la funcionalidad, lo cual no es un objetivo de la refactorización pura.

Los detalles de implementación sobre la refactorización pueden volverse bastante granulares. Hay muchos "malos olores" del código que podrían impulsar una refactorización, como métodos largos, clases grandes o código duplicado. Existen herramientas automatizadas que pueden ayudar en la refactorización, como "Clang-Tidy" para C++, que puede detectar problemas e incluso aplicar algunas correcciones.

Además, la refactorización requiere un conjunto sólido de pruebas para asegurar que la funcionalidad permanezca sin cambios. Sin pruebas, básicamente estás volando a ciegas y arriesgándote a regresiones.

## Ver también
Para una comprensión más profunda de la refactorización y para ver más ejemplos, podrías querer echar un vistazo a:

- El texto clásico de Martin Fowler "Refactoring: Improving the Design of Existing Code" para ideas fundamentales y estrategias.
- La documentación de `Clang-Tidy` en https://clang.llvm.org/extra/clang-tidy/ para soporte de refactorización automatizada en C++.
- "Working Effectively with Legacy Code" de Michael Feathers, que proporciona técnicas para refactorizar de manera segura en el contexto de bases de código existentes menos que perfectas.
