---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Convertir cadenas de texto a minúsculas en Python

## ¿Qué & Por qué?

Transformar una cadena de texto a minúsculas significa convertir todas las letras mayúsculas en minúsculas. Los programadores lo hacen para normalizar los datos de entrada y mejorar la eficiencia en la manipulación de las cadenas de texto.

## Cómo hacerlo:

Python proporciona un método incorporado llamado `.lower()` para manejar esta tarea. Veamos cómo este método funciona en un ejemplo sencillo.

```Python
string = "¡Hola Mundo!"
string_min = string.lower()
print(string_min)
```

```Output
¡hola mundo!
```

Como puedes ver, este código convierte todas las letras mayúsculas en minúsculas, permitiendo un manejo de datos consistente e insensible al caso.

## Inmersión Profunda

El método `.lower()` ha estado presente en Python desde muy temprano en su desarrollo, dado su papel fundamental en la manipulación de las cadenas de texto. 

También hay alternativas, como usar `str.casefold()`. Este método es aún más agresivo en la conversión a minúsculas, pudiendo manejar casos especiales en ciertos idiomas con caracteres que no existen en la tipografía inglesa.

En cuanto a la implementación, `.lower()` es bastante eficiente. Revisa cada carácter de la cadena y si es mayúscula, lo convierte. Si ya es minúscula, lo deja intacto. El coste de este proceso es proporcional al tamaño de la cadena.

## Ver También

No dejes de explorar estos recursos para aprender más sobre las operaciones con cadenas en Python:

1. [Python String lower() Method](https://www.w3schools.com/python/ref_string_lower.asp) - W3Schools
2. [Python String casefold() Method](https://www.w3schools.com/python/ref_string_casefold.asp) - W3Schools
3. [Text Sequence Type — str](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str) - Official Python Documentation

Recuerda, la manipulación de las cadenas de texto es fundamental en la programación, y dominarla es indispensable. ¡Sigue practicando!