---
title:    "Bash: Convirtiendo una cadena a minúsculas"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas es una tarea muy común en la programación. Puede ser necesario para aplicaciones web, para asegurarse de que los datos ingresados por los usuarios sean consistentes y se puedan comparar fácilmente. También puede ser útil al trabajar con datos almacenados en una base de datos, ya que a menudo se requiere que los datos estén en un formato estándar.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Bash, podemos utilizar el comando "tr" con el modificador "-s" seguido de las letras que deseamos convertir.

```Bash
echo "HOLA MUNDO" | tr '[:upper:]' '[:lower:]'
```

Este comando tomará la cadena de texto "HOLA MUNDO" y la convertirá a "hola mundo". También podemos guardar el resultado en una variable para usarlo más adelante en nuestro código.

```Bash
cadena="HOLA MUNDO"
resultado=$(echo "$cadena" | tr '[:upper:]' '[:lower:]')
echo "$resultado"
```

En este ejemplo, hemos guardado la cadena de texto "HOLA MUNDO" en la variable `cadena` y hemos utilizado el comando "tr" para convertirla a minúsculas, almacenándola en la variable `resultado`. Luego, hemos imprimido el resultado en la pantalla utilizando el comando "echo".

## Profundizando

Además del comando "tr", también podemos utilizar el comando "sed" para convertir una cadena de texto a minúsculas. El siguiente comando hará exactamente lo mismo que el ejemplo anterior:

```Bash
echo "HOLA MUNDO" | sed -e 's/\(.*\)/\L\1/'
```

Aquí, estamos utilizando la opción "-e" para especificar la expresión regular que se utilizará para realizar la conversión. La expresión regular especifica que todas las letras serán reemplazadas por su versión en minúsculas.

Ambos comandos también pueden utilizarse para convertir una cadena de texto a mayúsculas, simplemente cambiando los modificadores de "[:lower:]" a "[:upper:]" o la expresión regular "s/\(.*\)/\U\1/" en el comando "sed".

## Ver también

En este artículo, hemos aprendido cómo convertir una cadena de texto a minúsculas utilizando los comandos "tr" y "sed" en Bash. Si deseas profundizar más en el tema, puedes consultar estos recursos:

- [Documentación oficial de Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial de Bash en español](https://blog.desdelinux.net/tutorial-de-bash-en-espanol/)
- [Curso de Bash en Platzi](https://platzi.com/clases/bash/)