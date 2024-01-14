---
title:    "Bash: Impresión de salida de depuración"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué?

¿Alguna vez te has encontrado con errores en tus scripts de Bash y no sabes dónde está el problema? ¡La solución podría ser imprimir un poco de salida de depuración! Al imprimir información durante la ejecución del código, puedes identificar rápidamente dónde se encuentra el error y solucionarlo más fácilmente.

## Cómo hacerlo

Para imprimir salida de depuración en Bash, utilizamos el comando `echo` o `printf` seguido de la variable o mensaje que queremos imprimir. Veamos un ejemplo:

```Bash
# Crear una variable
my_var="¡Hola, mundo!"

# Imprimir la variable
echo $my_var
```

En este ejemplo, creamos una variable llamada `my_var` con el mensaje "¡Hola, mundo!" y luego utilizamos el comando `echo` para imprimirlo en la pantalla. Esto nos dará la salida `¡Hola, mundo!`, lo que nos asegura que la variable se ha creado correctamente.

También podemos imprimir varios valores o variables en la misma línea utilizando paréntesis y separando cada elemento con un espacio:

```Bash
# Crear dos variables
first_name="John"
last_name="Doe"

# Imprimir ambas variables en una sola línea
echo "Mi nombre es ${first_name} ${last_name}."
```

La salida será `Mi nombre es John Doe.`, lo que nos permite ver claramente cómo se han combinado ambas variables en la cadena de texto.

## Profundizando

Además de imprimir variables y mensajes, también podemos imprimir el resultado de comandos y expresiones en Bash. Utilizamos la expansión de comandos `$()` para ejecutar un comando y luego imprimir su salida. Veamos un ejemplo:

```Bash
# Obtener el nombre de usuario actual
current_user=$(whoami)

# Imprimir un mensaje que incluye el nombre de usuario
echo "El nombre de usuario actual es ${current_user}."
```

La salida será algo así como `El nombre de usuario actual es johndoe.` dependiendo de tu nombre de usuario.

También podemos utilizar la expansión aritmética `$((expression))` para imprimir el resultado de expresiones matemáticas. Por ejemplo:

```Bash
# Calcular el resultado de una expresión
result=$((5 + 3 * 2))

# Imprimir el resultado
echo "El resultado es ${result}."
```

La salida será `El resultado es 11.` ya que estamos calculando 5 más 3 veces 2.

Recuerda que puedes utilizar el comando `unset` para eliminar una variable y así evitar que se imprima su valor en el futuro.

## Ver también

Ahora que sabes cómo imprimir salida de depuración en Bash, ¡puedes utilizar esta técnica para resolver problemas en tus scripts y ahorrar tiempo de depuración! Aquí tienes algunos enlaces que podrían ser útiles para seguir aprendiendo:

- [¿Qué es Bash?](https://www.howtogeek.com/679666/what-is-bash-and-why-is-it-used-in-linux/)
- [Guía de Bash para principiantes](https://linuxize.com/post/bash-scripting-quick-start/)
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/manual/bash.html)