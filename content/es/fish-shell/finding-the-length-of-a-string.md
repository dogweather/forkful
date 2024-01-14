---
title:    "Fish Shell: Encontrando la longitud de una cadena"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por qué
Aprender a encontrar la longitud de una cadena de texto es una habilidad básica en la programación que puede ser útil en diferentes situaciones. Por ejemplo, puede ser útil para validar datos de entrada, para realizar operaciones en cadenas específicas o simplemente para obtener información sobre una cadena de texto determinada. 

## Cómo hacerlo
Para encontrar la longitud de una cadena de texto en Fish Shell, podemos utilizar el comando `string length` seguido del texto entre comillas. Por ejemplo: 

```Fish Shell
string length "¡Hola Mundo!"
```
El código anterior nos daría como resultado el número 11, ya que la cadena de texto tiene 11 caracteres. Podemos utilizar también variables en lugar de texto en el comando `string length`, siempre y cuando la variable contenga una cadena de texto. Por ejemplo: 

```Fish Shell
set texto "No hay lugar como el hogar"
string length $texto
```

En este caso, obtendremos como resultado el número 26, ya que la variable `texto` contiene una cadena de texto de 26 caracteres. 

## Profundizando
Para comprender mejor cómo funciona el comando `string length`, es importante tener en cuenta que, en Fish Shell, las cadenas de texto son tratadas como matrices de caracteres. Esto significa que cada letra en una cadena de texto tiene una posición específica en la matriz. 

Por ejemplo, si tenemos la cadena de texto "Hola", la letra "H" estaría en la posición 0, la letra "o" estaría en la posición 1, la letra "l" estaría en la posición 2 y la letra "a" estaría en la posición 3. 

Al utilizar el comando `string length`, lo que hace Fish Shell es contar el número de elementos en la matriz de caracteres que representa la cadena de texto. Es por eso que obtenemos un resultado de 26 en el segundo ejemplo anterior, ya que hay 26 caracteres en la cadena de texto "No hay lugar como el hogar".

# Ver también
- [Documentación oficial de Fish Shell sobre el comando `string length`](https://fishshell.com/docs/current/cmds/string-length.html)
- [Otro artículo sobre cómo encontrar la longitud de una cadena de texto en Fish Shell](https://medium.com/@hunterhector/how-to-get-the-length-of-a-string-in-fish-shell-9fe695b0848b)