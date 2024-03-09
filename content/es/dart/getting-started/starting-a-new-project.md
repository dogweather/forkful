---
title:                "Iniciando un nuevo proyecto"
date:                  2024-03-08T21:57:05.997315-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Iniciar un nuevo proyecto en Dart implica configurar un ambiente propicio para el desarrollo, pruebas e implementación eficientes. Los programadores inician nuevos proyectos Dart para aprovechar el óptimo rendimiento de Dart y su robusto ecosistema, particularmente para el desarrollo de aplicaciones web y móviles con frameworks como Flutter.

## Cómo:

1. **Instalar Dart**:
   Asegúrate de que Dart esté instalado en tu sistema. Si no, puedes descargarlo desde [https://dart.dev/get-dart](https://dart.dev/get-dart). Verifica la instalación con:

   ```shell
   dart --version
   ```

2. **Crear un Nuevo Proyecto Dart**:
   Usa la CLI de Dart para generar un nuevo proyecto:

   ```shell
   dart create hello_dart
   ```

   Este comando crea un nuevo directorio `hello_dart` con una simple aplicación de muestra web o de consola, dependiendo de tu selección.

3. **Examinar la Estructura del Proyecto**:
   
   Navega al directorio de tu proyecto:

   ```shell
   cd hello_dart
   ```

   Un proyecto Dart típico incluye los siguientes archivos y directorios clave:

   - `pubspec.yaml`: Archivo de configuración que incluye las dependencias de tu proyecto y las restricciones del SDK.
   - `lib/`: Directorio donde reside la mayoría del código Dart.
   - `test/`: Directorio para las pruebas del proyecto.

4. **Agregar Dependencias**:
   Edita `pubspec.yaml` para agregar dependencias. Para proyectos web, considera agregar `http`, un paquete popular para realizar solicitudes HTTP:

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   Después de editar, obtén las dependencias:

   ```shell
   dart pub get
   ```

5. **Escribe Tu Primer Código Dart**:
   
   En el directorio `lib/`, crea un nuevo archivo Dart, `main.dart`, y añade un simple código Dart:

   ```dart
   // Importa la biblioteca central de Dart
   import 'dart:core';

   void main() {
     print('¡Hola, Dart!');
   }
   ```

6. **Ejecuta Tu Aplicación Dart**:

   Ejecuta tu programa Dart con:

   ```shell
   dart run
   ```

   La salida debería ser:

   ```
   ¡Hola, Dart!
   ```

Siguiendo estos pasos, has iniciado con éxito un nuevo proyecto Dart, desde la instalación hasta ejecutar tu primer código Dart. Este conocimiento fundamental prepara el terreno para profundizar en el rico ecosistema de Dart y sus capacidades para construir aplicaciones escalables.
