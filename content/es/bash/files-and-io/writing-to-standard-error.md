---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:09.597875-07:00
description: "Escribir en el error est\xE1ndar (stderr) en Bash se trata de dirigir\
  \ mensajes de error o cualquier salida de diagn\xF3stico importante, separ\xE1ndola\
  \ de la\u2026"
lastmod: '2024-03-13T22:44:59.263457-06:00'
model: gpt-4-0125-preview
summary: "Escribir en el error est\xE1ndar (stderr) en Bash se trata de dirigir mensajes\
  \ de error o cualquier salida de diagn\xF3stico importante, separ\xE1ndola de la\u2026"
title: "Escribiendo en el error est\xE1ndar"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Escribir en el error estándar (stderr) en Bash se trata de dirigir mensajes de error o cualquier salida de diagnóstico importante, separándola de la salida estándar (stdout). Los programadores hacen esto para asegurarse de que los mensajes de error puedan ser fácilmente identificados, registrados o incluso ignorados, ayudando en los procesos de depuración y registro.

## Cómo hacerlo:
En Bash, se utiliza `>&2` para redirigir la salida hacia stderr. Aquí hay un ejemplo básico:

```bash
echo "Este es un mensaje normal"
echo "Este es un mensaje de error" >&2
```

Ejecutar este script mostrará ambos mensajes en la consola, pero si los rediriges, puedes separar el stdout del stderr. Por ejemplo:

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt` contendrá `"Este es un mensaje normal"`, mientras que `error.txt` capturará `"Este es un mensaje de error"`.

Para un caso de uso práctico, considera un script que procesa archivos e informa un error si un archivo no existe:

```bash
filename="ejemplo.txt"

if [ ! -f "$filename" ]; then
    echo "$filename no existe!" >&2
    exit 1
else
    echo "Procesando $filename"
fi
```

Salida de muestra directamente en la consola cuando `ejemplo.txt` no existe:

```
ejemplo.txt no existe!
```

No hay bibliotecas de terceros directas en Bash para manejar stderr, ya que la redirección cuenta con soporte nativo y generalmente es suficiente. Sin embargo, para aplicaciones complejas, se pueden incorporar marcos de registro o herramientas de registro externas como `syslog` o `log4bash` para gestionar tanto stdout como stderr de manera más efectiva.
