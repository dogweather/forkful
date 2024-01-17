---
title:                "Comprobando la existencia de un directorio"
html_title:           "Arduino: Comprobando la existencia de un directorio"
simple_title:         "Comprobando la existencia de un directorio"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Comprobar si un directorio existe es una función esencial para los programadores ya que les permite verificar si un directorio específico existe en la estructura de almacenamiento de su proyecto de Arduino. Esto es útil para evitar errores y asegurar que el código funcione correctamente.

## Cómo hacerlo:
```Arduino
// Verifica si el directorio existe
if (SD.exists("nombre_directorio")) {
  // Realiza alguna acción si existe
  Serial.println("El directorio existe.");
} else {
  // Realiza alguna acción si no existe
  Serial.println("El directorio no existe.");
}

// Crea un nuevo directorio
SD.mkdir("nuevo_directorio");

// Verifica si el directorio se creó correctamente
if (SD.exists("nuevo_directorio")) {
  Serial.println("El nuevo directorio se creó exitosamente.");
}
```

## Profundizando:
Comprobar la existencia de un directorio puede ser útil para organizar y acceder a archivos en tu proyecto de Arduino. Si el directorio no existe, puedes utilizar la función `mkdir` para crearlo. Otra forma de verificar la existencia de un directorio es utilizando la función `SD.open` para intentar abrir un archivo en ese directorio. Si la apertura es exitosa, entonces sabes que el directorio existe.

## Ver también:
- Documentación de `SD.exists()`: https://www.arduino.cc/en/Reference/SDExists
- Tutorial de creación y organización de directorios en una tarjeta SD: https://www.arduino.cc/en/Tutorial/ReadWrite