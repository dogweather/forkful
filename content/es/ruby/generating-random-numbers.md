---
title:    "Ruby: Generando números aleatorios"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por qué: Generar números aleatorios en Ruby

La generación de números aleatorios es una técnica importante en la programación de Ruby y se utiliza para simular resultados aleatorios en juegos, sorteos, entre otros. También es útil para crear datos de prueba y mejorar la seguridad en la generación de contraseñas y tokens de autenticación. En este artículo, exploraremos cómo generar números aleatorios en Ruby y por qué es importante para los programadores.

## Cómo hacerlo: Ejemplos de código y resultados

Generar números aleatorios en Ruby es muy sencillo gracias a la clase `Random` incorporada en el lenguaje. Veamos algunos ejemplos de cómo podemos utilizarla:

```
# Generar un número aleatorio entre 1 y 10
Random.rand(1..10)
# => 7

# Generar un número aleatorio entre 100 y 200
Random.rand(100..200)
# => 193

# Generar un número aleatorio entre 0 y 1
Random.rand()
# => 0.8146721521668552
```

Podemos también utilizar el método `seed` para establecer una semilla para generar los números aleatorios. De esta manera, si utilizamos la misma semilla, obtendremos los mismos números aleatorios cada vez que ejecutemos el programa. Esto puede ser útil para fines de prueba o para reproducir resultados en ciertas situaciones.

```
# Establecer una semilla para generar el mismo número aleatorio
srand(1234)
Random.rand()
# => 0.19151945037978274
```

## Profundizando: Más información sobre la generación de números aleatorios

Aunque Ruby hace que la generación de números aleatorios sea muy sencilla, es importante tener en cuenta que estos números no son completamente aleatorios. En realidad, se generan a partir de una fórmula matemática conocida como generador de números pseudoaleatorios. Esto significa que los números no son verdaderamente aleatorios y pueden ser reproducidos con la misma semilla.

Además, si no se establece una semilla, Ruby utilizará una semilla predeterminada basada en el tiempo actual del sistema. Esto puede ser un problema de seguridad, ya que un atacante puede potencialmente predecir los números aleatorios en ciertas situaciones.

Para mejorar la seguridad en la generación de números aleatorios, se recomienda utilizar la gema `SecureRandom` en lugar de la clase `Random`. Esta gema utiliza un generador de números aleatorios criptográficamente seguro y asegura que los números sean más impredecibles y adecuados para su uso en situaciones sensibles.

## Ver también

- [Documentación de la clase `Random`](http://ruby-doc.org/core-2.6/Random.html)
- [Documentación de la gema `SecureRandom`](https://ruby-doc.org/stdlib-2.6/libdoc/securerandom/rdoc/SecureRandom.html)
- [Artículo sobre generación de números aleatorios en Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/securerandom/rdoc/SecureRandom.html)