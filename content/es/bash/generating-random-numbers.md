---
title:    "Bash: Generando números aleatorios"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una habilidad esencial en la programación de Bash. Puede ser utilizado en diversos escenarios, desde juegos hasta pruebas de rendimiento de software. También es útil para crear datos de ejemplo y realizar experimentos. En esta entrada del blog, aprenderemos cómo generar números aleatorios en Bash y profundizaremos en cómo funciona este proceso.

## Cómo hacerlo

Para generar números aleatorios en Bash, utilizaremos el comando `shuf`. Este comando se utiliza para generar una secuencia aleatoria de números o cadenas de texto. Podemos especificar el rango de números con el argumento `-i` y la cantidad de números a generar con `-n`. Veamos un ejemplo:

```Bash
# Generar 5 números aleatorios entre 1 y 10
shuf -i 1-10 -n 5
```
El resultado podría ser el siguiente:
```
4
8
2
1
9
```
También podemos usar `shuf` para generar secuencias aleatorias de caracteres. Por ejemplo, para generar una contraseña aleatoria de 8 caracteres, podemos usar:

```Bash
# Generar una contraseña aleatoria de 8 caracteres
shuf -n 8 -e {a..z}
```

El resultado podría ser una contraseña como: `rpkwczal`.

## Inmersión profunda

Ahora sabemos cómo generar números y cadenas aleatorios en Bash, pero ¿cómo funciona en realidad? El comando `shuf` utiliza el generador de números aleatorios de Linux, llamado `/dev/urandom`. Este dispositivo se alimenta con entropía del sistema, como la actividad del mouse, la entrada del teclado y otros datos aleatorios. Esto garantiza que la secuencia generada sea verdaderamente aleatoria.

Sin embargo, es importante tener en cuenta que, si bien `/dev/urandom` es adecuado para la mayoría de los casos, no se recomienda para fines criptográficos. Para esos casos, se debe utilizar el generador de números aleatorios `/dev/random`, que utiliza una fuente de entropía más segura, aunque puede ser más lenta.

## Ver también

- Documentación oficial de `shuf`: https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html
- Uso de `/dev/random` y `/dev/urandom`: https://jameshfisher.com/2019/01/31/dev-random/
- Generador de claves seguras de Bash: https://www.shellhacks.com/generate-random-passwords/