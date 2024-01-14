---
title:    "Python: Uniendo cadenas de texto"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/concatenating-strings.md"
---

{{< edit_this_page >}}

# ¿Por qué deberías aprender a concatenar cadenas en Python?

Si eres un programador principiante en Python, probablemente te estés preguntando qué es la concatenación de cadenas y por qué es importante. La respuesta es simple: la concatenación de cadenas es una habilidad fundamental en la programación que te permitirá combinar y manipular cadenas de texto de manera efectiva. En este artículo, te explicaremos cómo hacerlo en Python y por qué es una habilidad valiosa para cualquier programador.

## Cómo hacerlo

La concatenación de cadenas en Python es una tarea sencilla. Primero, debes comprender cómo se crean y manipulan las cadenas en Python. Una cadena es simplemente un conjunto de caracteres encerrados entre comillas simples o dobles.

Para concatenar dos cadenas, simplemente las unes usando el operador `+`. Por ejemplo:

```Python
frase1 = "Hola"
frase2 = "mundo"
frase_unida = frase1 + " " + frase2

print(frase_unida)
```

El resultado será "Hola mundo". En este ejemplo, hemos creado dos variables que contienen cadenas y luego las hemos concatenado con un espacio en blanco entre ellas.

También puedes utilizar el método `.join()` para concatenar varias cadenas. Este método toma una lista de cadenas como argumento y las une en una sola cadena. Por ejemplo:

```Python
frase = " ".join(["¡Hola", "mundo", "!"])

print(frase)
```

El resultado será "¡Hola mundo!". En este caso, creamos una lista con todas las palabras de la frase y luego utilizamos el método `.join()` para unirlas en una sola cadena.

## Profundizando

Además de los métodos mencionados anteriormente, hay algunas cosas más que debes saber sobre la concatenación de cadenas en Python:

- No puedes concatenar una cadena con un número. Antes de unirlos, debes convertir el número en una cadena utilizando la función `str()`.
- Puedes utilizar el operador `*` para repetir una cadena un determinado número de veces. Por ejemplo, `"Hola" * 3` resultará en "HolaHolaHola".
- Si estás trabajando con cadenas largas, puedes utilizar el triple comillas `"""` para incluir saltos de línea en la cadena, lo que facilitará la lectura del código.

En resumen, la concatenación de cadenas es una habilidad esencial en la programación y en Python es muy sencilla de realizar. ¡Ahora que sabes cómo hacerlo, puedes empezar a experimentar con diferentes métodos y crear cadenas cada vez más complejas!

## Ver también

- [Documentación oficial de Python sobre cadenas](https://docs.python.org/es/3/library/stdtypes.html#text-sequence-type-str)
- [Tutorial de Programiz sobre concatenación de cadenas en Python](https://www.programiz.com/python-programming/string-concatenation)