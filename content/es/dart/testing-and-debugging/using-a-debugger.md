---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:04.696629-07:00
description: "C\xF3mo: **1. Establecer Puntos de Interrupci\xF3n:** Para establecer\
  \ un punto de interrupci\xF3n, simplemente haz clic en el margen izquierdo de la\
  \ l\xEDnea de\u2026"
lastmod: '2024-03-13T22:44:58.756925-06:00'
model: gpt-4-0125-preview
summary: ''
title: Usando un depurador
weight: 35
---

## Cómo:


### Depuración Básica:
**1. Establecer Puntos de Interrupción:**

Para establecer un punto de interrupción, simplemente haz clic en el margen izquierdo de la línea de código en tu IDE (p. ej., Visual Studio Code o Android Studio) donde deseas que se pause la ejecución.

```dart
void main() {
  var message = 'Hola, Depurando';
  print(message); // Establece un punto de interrupción aquí
}
```

**2. Iniciar Depuración:**

En tu IDE, inicia una sesión de depuración haciendo clic en el icono de depuración o presionando el botón de depuración. La ejecución se pausará en los puntos de interrupción.

**3. Inspeccionar Variables:**

Una vez que la ejecución se haya pausado, pasa el cursor sobre las variables para ver sus valores actuales.

**4. Avanzar Paso a Paso por el Código:**

Usa los comandos de avanzar sobre, avanzar dentro y avanzar fuera en tu IDE para navegar por tu código una línea o función a la vez.

### Depuración Avanzada con Observatory:
Dart incluye una herramienta llamada Observatory para la depuración y el perfilado de aplicaciones Dart. Es particularmente útil para aplicaciones que se ejecutan en la VM de Dart.

**Accediendo a Observatory:**

Ejecuta tu aplicación Dart con la bandera `--observe`.

```bash
dart --observe tu_programa.dart
```

Este comando imprime una URL en la consola, la cual puedes abrir en un navegador web para acceder al depurador Observatory.

### Uso de Bibliotecas de Terceros Populares:
Para la depuración de aplicaciones Flutter, el paquete `flutter_devtools` proporciona un conjunto de herramientas de rendimiento y depuración que se integran tanto con la VM de Dart como con Flutter.

**Instalación:**

Primero, agrega `devtools` a tu archivo `pubspec.yaml` bajo `dev_dependencies`:

```yaml
dev_dependencies:
  devtools: any
```

**Lanzando DevTools:**

Ejecuta este comando en tu terminal:

```bash
flutter pub global run devtools
```

Luego, inicia tu aplicación Flutter en modo de depuración. DevTools proporciona características como el inspector de Flutter para análisis del árbol de widgets y el perfilador de red para monitorear la actividad de la red.

### Salida de Ejemplo:
Al llegar a un punto de interrupción, tu IDE podría mostrar valores de variables y rastreos de pila de la siguiente manera:

```
message: 'Hola, Depurando'
```

Al aprovechar efectivamente las herramientas y técnicas de depuración en Dart, los desarrolladores pueden identificar y resolver problemas más rápidamente, lo que conduce a un proceso de desarrollo más fluido y aplicaciones más robustas.
