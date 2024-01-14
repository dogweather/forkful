---
title:                "Fish Shell: Comprobando si existe un directorio"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##¿Por qué comprobar si un directorio existe?

Comprobar si un directorio existe es una habilidad importante en la programación de Fish Shell. Esto se debe a que te permite tomar decisiones en tu código basadas en la existencia o no de un directorio específico. Esto es especialmente útil cuando estás creando scripts o automatizando tareas en tu sistema.

##Cómo hacerlo

Para comprobar si un directorio existe en tu sistema utilizando Fish Shell, puedes utilizar el siguiente comando:

```Fish Shell
if test -d "nombre_del_directorio"
	echo "El directorio existe"
else
	echo "El directorio no existe"
end
```

Al ejecutar este código, el resultado será "El directorio existe" si el directorio especificado existe en tu sistema o "El directorio no existe" si no es así.

También puedes utilizar la siguiente sintaxis, que es más corta y concisa:

```Fish Shell
if contains -q nombre_del_directorio
	echo "El directorio existe"
else
	echo "El directorio no existe"
end
```

Esta sintaxis utiliza la función `contains`, que devuelve `1` si el directorio existe y `0` si no.

##Profundizando

Comprobar si un directorio existe puede ser útil en diferentes situaciones. Por ejemplo, puedes utilizarlo para asegurarte de que un directorio necesario para tu script o programa exista antes de intentar acceder a él. También puedes utilizarlo para crear automáticamente un nuevo directorio si no existe.

Además, puedes utilizar el comando `test` o la función `contains` en conjunto con otras funciones y comandos de Fish Shell, como `cd` o `mkdir`, para crear scripts más avanzados y automatizar tareas en tu sistema.

¡Explora las diferentes formas en las que puedes utilizar comprobar si un directorio existe en tus proyectos y descubre cómo puede facilitar tu trabajo con Fish Shell!

##Ver también

- [Documentación de Fish Shell] (https://fishshell.com/docs/)
- [Tutorial de Fish Shell para principiantes] (https://dev.to/deven96/introduction-to-fish-shell-for-beginners-hd9)
- [Ejemplos de scripts de Fish Shell] (https://github.com/jorgebucaran/fisher)