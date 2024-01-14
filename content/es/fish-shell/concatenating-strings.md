---
title:    "Fish Shell: Uniendo cadenas"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por qué

La operación de concatenación de cadenas es una técnica fundamental en cualquier lenguaje de programación, incluyendo Fish Shell. Al combinar fragmentos de texto, podemos crear mensajes más completos y tener un mejor control sobre la salida de nuestros programas.

## Cómo hacerlo

La sintaxis básica para concatenar cadenas en Fish Shell es mediante el uso del operador `+`. Por ejemplo:

```
Fish Shell > set saludo "Hola"
Fish Shell > set nombre "amigos"
Fish Shell > echo $saludo" "$nombre
Hola amigos
```

También podemos incluir variables en la concatenación:

```
Fish Shell > set numero 42
Fish Shell > echo "La respuesta al universo es "$numero
La respuesta al universo es 42
```

En caso de que necesitemos agregar números o caracteres a una cadena, podemos usar la función `string` para convertirlos:

```
Fish Shell > set numero 3
Fish Shell > echo "La cuenta regresiva comienza en "$(string $numero)
La cuenta regresiva comienza en 3
```

## Profundizando

Fish Shell ofrece una gran cantidad de funciones útiles para manipular y concatenar cadenas. Podemos utilizar `string split` para dividir una cadena en varias partes, `string match` para encontrar una subcadena en otra, y `string sub` para reemplazar una parte de una cadena por otra. También podemos usar `strlen` para determinar la longitud de una cadena.

Además, Fish Shell admite el uso de expresiones regulares para la manipulación de cadenas, lo que nos permite realizar operaciones más complejas y específicas.

## Ver también

- Documentación oficial de Fish Shell sobre la concatenación de cadenas: https://fishshell.com/docs/current/cmds/set.html#description
- Ejemplos y casos de uso de concatenación de cadenas en Fish Shell: https://danielmiessler.com/study/fish/productivity/
- Tutorial en español sobre manipulación de cadenas en Fish Shell: https://medicstech.co/tutorial-fish-shell-en-espanol/