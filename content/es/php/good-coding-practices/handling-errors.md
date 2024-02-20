---
date: 2024-01-26 00:55:07.072183-07:00
description: "El manejo de errores en PHP se trata de administrar y responder a condiciones\
  \ que interrumpen el flujo normal de un programa, como archivos que faltan o\u2026"
lastmod: 2024-02-19 22:05:17.675441
model: gpt-4-1106-preview
summary: "El manejo de errores en PHP se trata de administrar y responder a condiciones\
  \ que interrumpen el flujo normal de un programa, como archivos que faltan o\u2026"
title: Manejo de errores
---

{{< edit_this_page >}}

## ¿Qué y por qué?
El manejo de errores en PHP se trata de administrar y responder a condiciones que interrumpen el flujo normal de un programa, como archivos que faltan o entradas de datos erróneas. Los programadores manejan errores para prevenir fallos y para ofrecer a los usuarios una experiencia más fluida.

## Cómo hacerlo:
En PHP, puedes gestionar errores utilizando bloques `try-catch`, y puedes personalizar el proceso con manejadores de errores personalizados y excepciones.

```php
// Ejemplo básico de try-catch
try {
  // Hacer algo arriesgado
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // Manejar el error
  echo "Error: " . $e->getMessage();
}

// Establecer un manejador de errores personalizado
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// Usar excepciones
class MyException extends Exception {}

try {
  // Hacer algo y lanzar una excepción personalizada
  throw new MyException("¡Error personalizado!");
} catch (MyException $e) {
  // Manejar la excepción personalizada
  echo $e->getMessage();
}

// Salida de muestra:
// Error: fopen(nonexistentfile.txt): failed to open stream: No such file or directory
// ¡Error personalizado!
```

## Profundizando
En el pasado, los errores de PHP estaban más relacionados con advertencias y notificaciones que no detenían la ejecución del script. A medida que el lenguaje maduraba, adoptó un manejo de errores orientado a objetos más robusto mediante la clase Exception introducida en PHP 5. Más tarde, PHP 7 salió con clases de Error que finalmente diferenciaban entre errores y excepciones.

Antes de los bloques `try-catch`, PHP utilizaba `set_error_handler()` para tratar los errores. `try-catch` es más limpio y moderno. Pero los manejadores de errores personalizados todavía tienen su lugar, especialmente para código legado o cuando necesitas capturar lo que normalmente serían errores no excepcionales.

La interfaz `Throwable` en PHP 7+ significa que, ya sea un Error o una Excepción, puedes capturar ambos. Esto es práctico porque ahora no te pierdes errores críticos en tiempo de ejecución, que eran más difíciles de rastrear antes.

Las alternativas fuera de los mecanismos incorporados de PHP incluyen bibliotecas y frameworks que vienen con sus propios sistemas de manejo de errores, ofreciendo más características como registro de errores en archivos o mostrando páginas de errores amigables para el usuario.

## Ver también
- Documentación oficial de PHP sobre Excepciones: https://www.php.net/manual/es/language.exceptions.php
- PHP The Right Way sobre reporte de errores: https://phptherightway.com/#error_reporting
- Manual de PHP sobre Manejo de Errores: https://www.php.net/manual/es/book.errorfunc.php
