---
title:                "Escribiendo en el error estándar"
aliases:
- /es/ruby/writing-to-standard-error/
date:                  2024-02-03T19:34:11.733505-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo en el error estándar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y por qué?
Escribir en el error estándar (stderr) en Ruby trata sobre dirigir mensajes de error o diagnósticos a un flujo de salida separado, distinto de la salida estándar (stdout). Los programadores hacen esto para diferenciar la salida regular del programa de los errores y la información de depuración, facilitando así el diagnóstico de problemas y el análisis de registros.

## Cómo hacerlo:
La biblioteca estándar de Ruby proporciona una manera sencilla de escribir en stderr usando `$stderr` o `STDERR`. No necesitas bibliotecas de terceros para esta operación básica.

### Escribiendo un mensaje simple a stderr:
```ruby
$stderr.puts "Error: Archivo no encontrado."
# O de manera equivalente
STDERR.puts "Error: Archivo no encontrado."
```
Salida de muestra (a stderr):
```
Error: Archivo no encontrado.
```

### Redirigiendo stderr a un archivo:
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "Fallo al abrir configuración."
end
```
Este fragmento de código redirige stderr a un archivo llamado `error.log`, y todos los errores escritos subsiguientes serán dirigidos allí hasta que el programa restablezca la redirección de stderr o termine.

### Usando stderr con manejo de excepciones:
```ruby
begin
  # Simulando una operación que podría fallar, por ejemplo, abrir un archivo
  File.open('archivo_inexistente.txt')
rescue Exception => e
  STDERR.puts "Ocurrió una excepción: #{e.message}"
end
```
Salida de muestra (a stderr):
```
Ocurrió una excepción: No such file or directory @ rb_sysopen - archivo_inexistente.txt
```

Aunque los métodos integrados de Ruby para escribir en stderr son suficientes para muchas aplicaciones, para necesidades de registro más complejas, podrías considerar la biblioteca estándar `logger` o gemas externas como `Log4r`. Estas proporcionan mecanismos de registro configurables, incluyendo niveles de severidad, formatos, y la capacidad de escribir en varias salidas, incluyendo archivos, correo electrónico, y más.
