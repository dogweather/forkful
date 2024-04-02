---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:30.340097-07:00
description: "El an\xE1lisis de HTML en la programaci\xF3n implica extraer datos de\
  \ documentos HTML. Los programadores hacen esto para interactuar o raspar el contenido\
  \ web\u2026"
lastmod: '2024-03-13T22:44:58.748092-06:00'
model: gpt-4-0125-preview
summary: "El an\xE1lisis de HTML en la programaci\xF3n implica extraer datos de documentos\
  \ HTML. Los programadores hacen esto para interactuar o raspar el contenido web\u2026"
title: Analizando HTML
weight: 43
---

## ¿Qué y Por Qué?
El análisis de HTML en la programación implica extraer datos de documentos HTML. Los programadores hacen esto para interactuar o raspar el contenido web para la extracción de información, pruebas o propósitos de automatización, incluso cuando no hay APIs oficiales disponibles.

## Cómo hacerlo:
Dart no proporciona soporte integrado para el análisis de HTML en sus bibliotecas centrales. Sin embargo, puedes usar un paquete de terceros como `html` para analizar y manipular documentos HTML.

Primero, añade el paquete `html` a tu archivo `pubspec.yaml`:

```yaml
dependencies:
  html: ^0.15.0
```

Luego, importa el paquete en tu archivo Dart:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Aquí tienes un ejemplo básico de cómo analizar una cadena que contiene HTML y extraer datos:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Hola, Dart!</h1>
      <p>Este es un párrafo en un HTML de ejemplo</p>
    </body>
  </html>
  """;

  // Analizar la cadena HTML
  Document document = parse(htmlDocument);

  // Extracción de datos
  String titulo = document.querySelector('h1')?.text ?? "No se encontró título";
  String parrafo = document.querySelector('p')?.text ?? "No se encontró párrafo";

  print('Título: $titulo');
  print('Párrafo: $parrafo');
}
```

Salida:

```
Título: Hola, Dart!
Párrafo: Este es un párrafo en un HTML de ejemplo
```

Para interactuar con páginas web del mundo real, podrías combinar el análisis de `html` con solicitudes HTTP (usando el paquete `http` para obtener contenido web). Aquí tienes un ejemplo rápido:

Primero, añade el paquete `http` junto con `html`:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Luego, busca y analiza una página HTML de la web:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Obtener la página web
  var respuesta = await http.get(Uri.parse(url));
  
  if (respuesta.statusCode == 200) {
    var documento = parse(respuesta.body);

    // Suponiendo que la página tiene etiquetas <h1> que te interesan
    var titulares = documento.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Titulares: $titulares');
  } else {
    print('La solicitud falló con el estado: ${respuesta.statusCode}.');
  }
}
```

Nota: La técnica de raspado web mostrada arriba debe ser utilizada responsablemente y en cumplimiento con los términos de servicio del sitio web.
