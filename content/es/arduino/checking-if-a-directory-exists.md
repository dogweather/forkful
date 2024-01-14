---
title:    "Arduino: Comprobando si existe un directorio"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Hay muchas razones por las que uno podría querer verificar si un directorio existe en su proyecto de Arduino. Una de las razones más comunes es asegurarse de que los archivos necesarios se encuentren en la ubicación correcta antes de ejecutar un programa.

## Cómo hacerlo

Para verificar si un directorio existe en Arduino, podemos utilizar la función de la biblioteca `File.exists()`. Esta función tomará como argumento la ruta del directorio que deseamos verificar y devolverá un booleano que indica si el directorio existe o no.

```Arduino
if (SPIFFS.exists("/my_directory")) {
  Serial.println("El directorio existe");
} else {
  Serial.println("El directorio no existe");
}
```

En este ejemplo, estamos utilizando la biblioteca `SPIFFS` para acceder al sistema de archivos en la placa de Arduino. Primero, llamamos a la función `exists()` de la biblioteca, y le pasamos la ruta del directorio que queremos verificar. Luego, usamos un condicional para imprimir un mensaje en función del valor booleano devuelto.

## Profundizando

Si deseamos hacer una verificación más completa, podemos utilizar la función `SPIFFS.openDir()` para obtener un directorio y luego recorrer todos los archivos y subdirectorios dentro de él. Podemos combinar esto con la función `SPIFFS.exists()` para verificar cada archivo o subdirectorio individualmente.

```Arduino
Dir dir = SPIFFS.openDir("/my_directory");
while (dir.next()) {
  if (dir.fileAttributes() == "D") {
    Serial.println(dir.fileName() + " es un directorio.");
  } else {
    Serial.println(dir.fileName() + " es un archivo.");
  }
}
```

En este ejemplo, estamos obteniendo un objeto de tipo `Dir` llamado `dir` mediante la función `openDir()`. Luego, utilizamos un bucle `while` para recorrer todos los archivos y subdirectorios dentro del directorio. Podemos utilizar la función `fileAttributes()` para verificar si cada elemento es un directorio (valor "D") o un archivo (valor "F"). También podemos obtener el nombre de cada elemento utilizando la función `fileName()`. Esto nos permite realizar una verificación más detallada del contenido de un directorio específico.

## Ver también

- [Documentación de la biblioteca SPIFFS](https://arduino-esp8266.readthedocs.io/en/latest/filesystem.html)
- [Tutorial de Arduino: acceder al sistema de archivos SPIFFS](https://randomnerdtutorials.com/esp8266-nodemcu-vs-code-platformio-spiffs/)