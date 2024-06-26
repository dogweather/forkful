---
date: 2024-01-20 17:44:43.915293-07:00
description: "C\xF3mo hacerlo: Usaremos `file_get_contents` para agarrar el contenido\
  \ y `file_put_contents` para guardarlo. Aqu\xED un ejemplo."
lastmod: '2024-03-13T22:44:59.159679-06:00'
model: gpt-4-1106-preview
summary: Usaremos `file_get_contents` para agarrar el contenido y `file_put_contents`
  para guardarlo.
title: "Descargando una p\xE1gina web"
weight: 42
---

## Cómo hacerlo:
Usaremos `file_get_contents` para agarrar el contenido y `file_put_contents` para guardarlo. Aquí un ejemplo:

```PHP
<?php
$url = "http://ejemplo.com";
$paginaWeb = file_get_contents($url);

// Verificar que hemos recibido contenido
if ($paginaWeb !== false) {
    // Guardar contenido en un archivo local
    file_put_contents("pagina_descargada.html", $paginaWeb);
    echo "Descarga completada.";
} else {
    echo "Error al descargar la página.";
}
?>
```
Si corres este código, deberías ver "Descarga completada." y encontrar un archivo `pagina_descargada.html` en el mismo directorio de tu script.

## Deep Dive:
Antes, usar `fopen` y un bucle para leer línea por línea era común. Hoy, `file_get_contents` simplifica las cosas. Otra alternativa es cURL, útil para peticiones más complejas, como enviar headers o usar métodos HTTP distintos.

Es importante manejar errores, tal como muestra el ejemplo. La función puede devolver `false` si la URL está mal o si hay un problema de red. Usar `file_get_contents` puede no ser lo mejor para páginas enormes; en esos casos, cURL es más eficiente.

En términos de seguridad, ten cuidado al descargar contenido de fuentes desconocidas. Siempre sanitiza y valida lo que tu script procesa.

## Ver También:
- PHP Manual on `file_get_contents`: https://www.php.net/manual/es/function.file-get-contents.php
- PHP Manual on `file_put_contents`: https://www.php.net/manual/es/function.file-put-contents.php
- cURL en PHP: https://www.php.net/manual/es/book.curl.php
- HTTP client Guzzle (una alternativa moderna): http://docs.guzzlephp.org/en/stable/
