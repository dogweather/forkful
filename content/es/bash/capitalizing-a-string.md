---
title:    "Bash: Capitalizar una cadena"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# ¿Por qué capitalizar una cadena de texto?

Si eres nuevo en la programación con Bash, es posible que te hayas encontrado con la necesidad de capitalizar una cadena de texto en algún momento. Esto puede ser útil para mejorar la legibilidad de tu código o para cumplir con los requisitos de formato de una aplicación. En este artículo, te explicaremos cómo capitalizar una cadena de texto en Bash y te daremos una visión más profunda sobre cómo funciona este proceso.

## Cómo hacerlo

En Bash, hay varias formas de capitalizar una cadena de texto. Uno de los métodos más sencillos es utilizar el comando `tr` (traducir), que permite reemplazar un conjunto de caracteres por otro.

Por ejemplo, para capitalizar la primera letra de una cadena de texto, puedes utilizar el siguiente comando:

```bash
echo "hola, ¿cómo estás?" | tr '[:lower:]' '[:upper:]'
```

Este comando tomará la cadena de texto "hola, ¿cómo estás?" y la convierte en "Hola, ¿cómo estás?".

Otra opción es utilizar el comando `sed` (editor de secuencias) para capitalizar cada palabra en una cadena de texto. Aquí hay un ejemplo:

```bash
echo "hola, ¿cómo estás?" | sed 's/\b\(.\)/\u\1/g'
```

Este comando tomará la cadena de texto y la convierte en "Hola, ¿Cómo Estás?".

## Profundizando en el tema

Ahora que ya sabes cómo capitalizar una cadena de texto en Bash, puede que te preguntes ¿qué pasa si quisieras capitalizar solo una parte específica de la cadena? Por ejemplo, es posible que solo quieras capitalizar la primera letra de una palabra o solo algunas palabras en particular.

En esos casos, lo más recomendable es utilizar un bucle `for` dentro de un bucle `while` para iterar sobre cada palabra en la cadena y capitalizar solo las palabras deseadas. Aquí hay un ejemplo de esto:

```bash
cadena="hola, ¿cómo estás? Bienvenido a Bash"

for palabra in $cadena; do
    primeraletra=$(echo "${palabra:0:1}" | tr '[:lower:]' '[:upper:]')
    restopalabra=$(echo "${palabra:1}" | tr '[:upper:]''[:lower:]')
    nuevapalabra=$primeraletra$restopalabra
    nueva_cadena+="$nuevapalabra "
done

echo $nueva_cadena
```

Este comando tomará la cadena de texto y solo capitalizará la primera letra de cada palabra, dejando el resto de las letras en minúsculas. El resultado sería: "Hola, ¿Cómo Estás? Bienvenido A Bash".

## Ver también
- [Comandos de Bash](https://www.gnu.org/software/bash/)
- [Guía de Bash para principiantes](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Documentación de Bash](https://tldp.org/LDP/Bash-Beginners-Guide/html/)