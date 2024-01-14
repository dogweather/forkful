---
title:    "Bash: Extrayendo subcadenas"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad importante en la programación Bash que puede ayudar a simplificar y automatizar tareas. Al extraer una parte específica de una cadena de texto, podemos realizar operaciones en ella más fácilmente y ahorrar tiempo y esfuerzo.

## Cómo hacerlo

Para extraer subcadenas en Bash, podemos usar el operador de shell "substr", que toma tres argumentos: la cadena original, la posición inicial y la longitud de la subcadena que queremos extraer.

Por ejemplo, si queremos extraer la palabra "programación" de la cadena "Aprender programación es divertido", podemos usar el siguiente comando:

```
texto="Aprender programación es divertido"
echo ${texto:9:12}
```

La posición inicial siempre comienza en 0, por lo que especificamos la posición 9 para comenzar en la letra "p" y la longitud de la subcadena es 12, ya que "programación" tiene 12 caracteres.

La salida de este comando sería "programación", ya que hemos extraído la subcadena desde la posición 9 hasta la posición 20.

## Inmersión profunda

Además del operador "substr", Bash también ofrece otras opciones para extraer subcadenas, como usar el comando "awk" o usar expresiones regulares con el comando "grep".

También podemos usar variables y loops para extraer subcadenas de manera dinámica y realizar operaciones en ellas. Esto es especialmente útil cuando tenemos una gran cantidad de datos y queremos extraer información específica de forma eficiente.

En general, es importante entender el funcionamiento de cada método de extracción de subcadenas y elegir el que mejor se adapte a nuestras necesidades.

## Ver también

- [Tutorial de Bash en español](https://www.linuxtotal.com.mx/index.php?cont=info_admon_003)
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/)
- [Ejemplos de extracción de subcadenas en Bash](https://www.baeldung.com/linux/bash-script-extract-string) (en inglés)