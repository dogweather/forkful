---
title:    "Fish Shell: Comprobando si existe un directorio"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Comprobar si un directorio existe puede ser una tarea importante en la programación de Fish Shell. Esta acción nos permite verificar si un directorio específico existe antes de intentar realizar alguna operación en él, lo cual puede ahorrar tiempo y evitar errores en nuestro código.

## Cómo hacerlo

Para comprobar si un directorio existe en Fish Shell, utilizamos el comando `test` seguido del indicador `-d` y la ruta del directorio que queremos verificar. Por ejemplo:

```Fish Shell
test -d /home/usuario/documentos 
```

Si el directorio existe, este comando nos devolverá un valor `verdadero` (true) y si no existe, nos devolverá un valor `falso` (false).

También podemos utilizar la sintaxis abreviada `[[ -d directorio ]]` para realizar la misma acción en una sola línea. Ejemplo:

```Fish Shell
if [[ -d /home/usuario/documentos ]]
  echo "El directorio existe"
end
```

## Profundizando

En el código mencionado anteriormente, utilizamos el indicador `-d` para comprobar si un directorio existe. Sin embargo, también existen otros indicadores que podemos utilizar con el comando `test` para realizar diferentes comprobaciones, como por ejemplo `-f` para verificar si un archivo existe y `-e` para comprobar si un archivo o directorio existe.

Además, se pueden combinar comandos `test` utilizando los operadores lógicos `&&` (y), `||` (o) o `!` (no) para realizar acciones más complejas.

## Ver también

- [Documentación de test en Fish Shell](https://fishshell.com/docs/current/cmds/test.html)
- [Uso de comandos lógicos en Fish Shell](https://fishshell.com/docs/current/tutorial.html#logical-operators)
- [Tutorial de inicio rápido de Fish Shell](https://fishshell.com/docs/current/tutorial.html)