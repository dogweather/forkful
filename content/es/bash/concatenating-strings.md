---
title:    "Bash: Concatenando cadenas"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar concatenación de cadenas en Bash? 

La concatenación de cadenas es una técnica importante en la programación Bash que permite unir varias cadenas de texto en una sola. Esto puede ser útil en una gran variedad de situaciones, como la creación de nombres de archivos dinámicos, la generación de mensajes personalizados, o la construcción de una URL a partir de parámetros.

## Cómo hacerlo

La concatenación de cadenas en Bash es sencilla y se puede realizar de varias maneras. Una forma es utilizando el operador `+` dentro de una variable, como se muestra a continuación:

```Bash
nombre="Juan"
edad=30
echo "¡Hola, mi nombre es $nombre y tengo $edad años!"
```

El resultado de este código sería: 

```
¡Hola, mi nombre es Juan y tengo 30 años!
```

Otra forma es utilizando el comando `printf`, como se muestra a continuación:

```Bash
apellido="Pérez"
direccion="Calle Mayor"
printf "Mi nombre completo es %s y vivo en la %s." "$nombre $apellido" "$direccion"
```

El resultado de este código sería:

```
Mi nombre completo es Juan Pérez y vivo en la Calle Mayor.
```

## Profundizando en la concatenación de cadenas 

En Bash, las cadenas se pueden concatenar utilizando el operador `+`, pero también se puede utilizar el comando `concat` para unir varias cadenas. Además, es importante tener en cuenta que las variables deben estar entre comillas dobles para que se puedan expandir dentro de la cadena concatenada.

También es posible concatenar más de dos cadenas utilizando el operador `+=`, como se muestra en el siguiente ejemplo:

```Bash
saludo="¡Hola"
saludo+=" a todos!"
echo $saludo
```

El resultado de este código sería: 

```
¡Hola a todos!
```

Otra técnica útil es la de utilizar el comando `read` para obtener la entrada del usuario y concatenarla con una cadena, como se muestra a continuación:

```Bash
echo "Por favor, ingresa tu nombre:"
read nombre
saludo="¡Hola, $nombre!"
echo $saludo
```

El resultado de este código sería:

```
Por favor, ingresa tu nombre: 
María
¡Hola, María!
```

## Ver también 

Si deseas aprender más sobre la concatenación de cadenas en Bash, te recomiendo que consultes los siguientes recursos:

- https://linuxize.com/post/bash-concatenate-strings/
- https://www.geeksforgeeks.org/bash-script-write/
- https://stackoverflow.com/questions/4181703/how-to-concatenate-string-variables-in-bash