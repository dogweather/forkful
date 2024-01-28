---
title:                "Registro de Actividades en Programación"
date:                  2024-01-26T01:03:27.732027-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Actividades en Programación"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/logging.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Registrar es esencialmente anotar lo que tu aplicación está haciendo: un diario, por así decirlo, pero para el código. Los programadores lo hacen para llevar un seguimiento de los detalles, como los cambios de estado, eventos del sistema y errores molestos, asegurándose de que ningún contratiempo pase desapercibido.

## Cómo hacerlo:
En Fish, registrar puede ser tan simple como redirigir las salidas estándar y de error a un archivo. Vamos a hacer una entrada en el registro para los tiempos de inicio y finalización de nuestro script.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script iniciado" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script finalizado" >> my_app.log
end

log_start
# ... las tareas de tu script ...
log_end

cat my_app.log
```

Esto es lo que verías en `my_app.log`:

```
2023-04-01 10:35:47  - Script iniciado
2023-04-01 10:36:02  - Script finalizado
```

Para un registro avanzado, puedes utilizar funciones con parámetros para el nivel de registro y mensajes:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "Este es un mensaje informativo."
log_message ERROR "¡Algo salió mal!"
```

La salida de muestra de `my_app.log` será:
```
2023-04-01 10:35:47 [INFO] Este es un mensaje informativo.
2023-04-01 10:35:49 [ERROR] ¡Algo salió mal!
```

## Profundización
Históricamente, el registro en scripts de shell se hacía con una serie de sentencias `echo`, y aunque esto ciertamente sigue siendo una opción, implementar sistemas más complejos puede ser un desafío. Fish no tiene un mecanismo de registro integrado como otros shells o lenguajes de programación, por lo que a menudo necesitas crear el tuyo propio.

Alternativas al comando `echo` integrado de Fish para el registro incluyen herramientas de Unix como `syslog` o `logger`, que se interfazan con el demonio del sistema de registro, proporcionando un enfoque más integrado para registrar eventos en todo el sistema.

La simplicidad de Fish te permite crear funciones para manejar la verbosidad del registro, estableciendo diferentes niveles que puedes activar o desactivar. Algunas implementaciones incluso pueden incluir el nombre del script, el número de línea y la fecha y hora, lo que facilita rastrear los pasos que llevaron a un evento.

## Consulta también
- La documentación de Fish Shell sobre la escritura de funciones: https://fishshell.com/docs/current/#syntax-function
- Consejos Básicos para Scripting en Shell: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Guía del Protocolo Syslog: https://tools.ietf.org/html/rfc5424
