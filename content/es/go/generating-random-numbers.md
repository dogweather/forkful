---
title:    "Go: Generación de números aleatorios"
keywords: ["Go"]
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios en Go?

La generación de números aleatorios es una técnica fundamental en la programación, ya que permite simular situaciones de incertidumbre y tomar decisiones de manera aleatoria. En Go, existen varias formas de generar números aleatorios, lo que ofrece una gran versatilidad a la hora de trabajar con este tipo de datos.

## Cómo hacerlo en Go

Hay dos maneras principales de generar números aleatorios en Go: utilizando el paquete `math/rand` y utilizando el paquete `crypto/rand`.

Con `math/rand`, podemos utilizar la función `Intn(n)` para generar un número aleatorio entre 0 y n-1. Por ejemplo, si queremos generar un número aleatorio entre 0 y 100, podemos utilizar `rand.Intn(100)`. Asimismo, para generar un número aleatorio en un rango específico, podemos utilizar la función `RandInt(min, max)`, donde min es el número mínimo y max es el número máximo del rango.

Por otro lado, con `crypto/rand` podemos generar números criptográficamente seguros utilizando la función `Read(b []byte)`. Esta función lee bytes aleatorios del generador criptográfico del sistema y los almacena en un slice de bytes pasado como argumento.

A continuación, se muestran ejemplos de ambas opciones:

```Go
// Generar un número aleatorio entre 0 y 100
fmt.Println(rand.Intn(100))

// Generar un número aleatorio entre 10 y 20
fmt.Println(rand.Intn(11) + 10)

// Generar un número criptográficamente seguro entre 0 y 255
b := make([]byte, 1)
rand.Read(b)
fmt.Println(int(b[0]))
```

## Profundizando en la generación de números aleatorios en Go

En Go, el paquete `math/rand` utiliza un generador lineal congruencial (LCG) para generar números aleatorios. Este tipo de generador funciona al tomar un valor inicial, llamado semilla, y aplicar una serie de operaciones matemáticas para obtener el siguiente número aleatorio en una serie. Por lo tanto, si utilizamos la misma semilla, obtendremos la misma secuencia de números aleatorios.

Es importante tener en cuenta que el generador LCG de `math/rand` no es criptográficamente seguro, ya que es posible predecir los números generados si se conoce la semilla inicial. Por lo tanto, si se requieren números aleatorios para fines criptográficos, es recomendable utilizar el paquete `crypto/rand`.

En resumen, la generación de números aleatorios en Go es una tarea sencilla que ofrece una gran flexibilidad y opciones. Sin embargo, es importante elegir el método adecuado según nuestras necesidades y considerar la seguridad en todo momento.

## Ver también

- Documentación del paquete `math/rand`: https://golang.org/pkg/math/rand/
- Documentación del paquete `crypto/rand`: https://golang.org/pkg/crypto/rand/