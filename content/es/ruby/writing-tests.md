---
title:                "Ruby: Escribiendo pruebas"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

¿Por qué es importante escribir pruebas en tu código de Ruby? Las pruebas son una parte fundamental del proceso de desarrollo de software, ya que te permiten asegurarte de que tu código funciona correctamente y sigue funcionando correctamente a medida que vas haciendo cambios. Es una forma de garantizar la calidad de tu código y evitar errores y fallos en el futuro.

## Cómo hacerlo

Antes de abordar el tema de cómo escribir pruebas en Ruby, primero hay que entender algunos conceptos básicos. En Ruby, las pruebas unitarias se escriben utilizando una biblioteca llamada "Minitest", que viene incluida con la instalación estándar de Ruby. Esta biblioteca proporciona una serie de métodos y funcionalidades para crear y ejecutar pruebas.

Para empezar, vamos a crear una clase simple de Ruby que represente a una persona. Dentro de esta clase, vamos a definir un método que calcule la edad de la persona basándose en su fecha de nacimiento. Aquí tienes el código:

```Ruby
class Persona
  def initialize(nombre, apellido, fecha_de_nacimiento)
    @nombre = nombre
    @apellido = apellido
    @fecha_de_nacimiento = fecha_de_nacimiento
  end

  def calcular_edad
    hoy = Date.today
    edad = hoy.year - @fecha_de_nacimiento.year
    if hoy.month < @fecha_de_nacimiento.month ||
    (hoy.month == @fecha_de_nacimiento.month && hoy.day < @fecha_de_nacimiento.day)
      edad -= 1
    end
    edad
  end
end
```

Ahora vamos a escribir una prueba para este método. Para hacerlo, utilizaremos la sintaxis de Minitest y la ejecutaremos en la consola de Ruby. Aquí tienes el código:

```Ruby
require 'minitest/autorun'
require_relative 'persona'

class PersonaTest < Minitest::Test
  def setup
    @persona = Persona.new("Juan", "Pérez", Date.new(1990, 3, 14))
  end

  def test_calcular_edad
    assert_equal 31, @persona.calcular_edad
  end
end
```

En este ejemplo, hemos utilizado un método de Minitest llamado `assert_equal` para comprobar que el resultado de llamar al método `calcular_edad` en nuestra instancia de `Persona` es igual a 31 (suponiendo que hoy es el 14 de marzo de 2021). Si todo funciona correctamente, obtendremos una salida de prueba exitosa.

## Profundizando

Escribir pruebas no es solo una cuestión de asegurar que tu código funcione correctamente en un momento dado, sino también de asegurarse de que seguirá funcionando correctamente en el futuro. Por eso es importante cubrir diferentes casos de uso en tus pruebas y escribir pruebas que sean fáciles de entender y mantener.

En nuestro ejemplo anterior, podríamos mejorar nuestras pruebas cubriendo diferentes casos de uso, como por ejemplo si la persona ha nacido en un año bisiesto o si la fecha de nacimiento está en el futuro. Además, podemos utilizar otros métodos de Minitest como `assert_raises` para comprobar que nuestro código maneja correctamente errores y excepciones.

También es importante establecer una buena estructura para tus pruebas, de manera que sea fácil de entender y mantener. Puedes agrupar tus pruebas en diferentes contextos utilizando la sintaxis `describe` y `it` de Minitest, y puedes utilizar la opción `focus` para centrarte en una prueba en particular mientras trabajas en ella.

## Ver también

Aquí tienes algunos enlaces útiles para aprender más sobre cómo escribir pruebas en Ruby con Minitest:

- [Documentación oficial de Minitest](https://github.com/seattlerb/minitest)
- [Introduction to Minitest for Ruby](https://semaphoreci.com/community/tutorials/introduction-to-minitest-for-ruby)
- [Ruby Testing: MiniTest & RSpec](https://thoughtbot.com/blog/ruby-testing-minitest-rspec)

¡Espero que este artículo te haya sido útil para entender la importancia de escribir pruebas en tu código de Ruby y cómo hacerlo con Minitest! ¡No dudes en explorar más sobre este tema y comenzar a implementar pruebas en tu propio código!