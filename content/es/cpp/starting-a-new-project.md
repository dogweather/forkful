---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "C++"
category:             "C++"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Iniciar un nuevo proyecto en programación significa crear una base desde cero para desarrollar una idea o solución. Los programadores lo hacen para implementar nuevas funcionalidades, solucionar problemas o simplemente aprender y experimentar con nuevos conceptos.

## ¿Cómo Hacerlo?

Aquí un pequeño ejemplo de un programa sencillo en C++. Un "Hola, Mundo!" para mostrar su sintaxis básica.

```C++
#include<iostream>

int main() {

    std::cout << "¡Hola, Mundo!";
    return 0;

}
```
Cuando lo compilas y ejecutas, verás lo siguiente:

```C++
¡Hola, Mundo!
```

## Profundizando

Iniciar un nuevo proyecto no siempre significa empezar desde cero. En muchos casos, puedes aprovechar librerías y marcos de trabajo existentes.

Históricamente, los proyectos en C++ comenzaban completamente desde cero. La programación orientada a objetos (OOP) introdujo la idea de la reutilización del código, lo que permitió a los programadores empezar a desarrollar sobre el trabajo de otros.

Existen alternativas al iniciar un nuevo proyecto desde cero. Puedes clonar un repositorio existente de Github, o utilizar generadores de proyectos como CMake. Aunque cada uno tiene sus pros y contras, lo importante es seleccionar el camino que más se ajuste a tus necesidades.

En cuanto a los detalles de implementación, la forma en que inicies tu proyecto dependerá en gran parte de tu entorno de desarrollo integral (IDE). Por ejemplo, en Visual Studio puedes hacer click en `File > New > Project` para iniciar uno. Luego seleccionas la plantilla `C++` y continuas con el asistente.

## Ver También

1. Tutorial Básico de C++: http://www.cplusplus.com/doc/tutorial/
2. C++ en Github: https://github.com/topics/cpp
3. Tutorial de CMake: https://cliutils.gitlab.io/modern-cmake/chapters/basics.html
4. Guía de Visual Studio para C++: https://docs.microsoft.com/es-es/cpp/cpp/?view=msvc-160