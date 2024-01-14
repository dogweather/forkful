---
title:    "Ruby: Escribiendo pruebas"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por qué

Escribir pruebas en Ruby puede parecer una tarea tediosa y poco emocionante, especialmente cuando se está ansioso por comenzar a codificar la lógica de tu programa. Sin embargo, estas pruebas son una parte crucial del proceso de desarrollo de software, ya que tienen una gran cantidad de beneficios. Las pruebas pueden ayudar a evitar errores comunes, mejorar la calidad del código y hacer el proceso de desarrollo más eficiente a largo plazo.

## Cómo hacerlo

Hay varias formas de escribir pruebas en Ruby, pero una de las herramientas más populares es RSpec. Esta gema hace que sea fácil escribir y ejecutar pruebas en un formato claro y legible. Para comenzar a usar RSpec, primero debes instalarlo en tu proyecto:

```ruby
gem install rspec
```

Luego, puedes crear un archivo de prueba con la extensión `.spec.rb` y comenzar a escribir tus pruebas. Por ejemplo, si queremos probar una función que devuelve la suma de dos números, podríamos escribir lo siguiente:

```ruby
# spec/calculadora_spec.rb

require 'calculadora'

RSpec.describe Calculadora do
  describe "#sumar" do
    it "devuelve la suma de dos números" do
      expect(Calculadora.sumar(2, 3)).to eq(5)
    end
  end
end
```

En este código, estamos definiendo una clase `Calculadora` que tiene un método `sumar` que acepta dos números como argumentos y devuelve su suma. Luego, en nuestras pruebas, usamos la expectativa `to eq` para verificar que la función devuelve el resultado esperado.

Una vez que hayas escrito tus pruebas, puedes ejecutarlas escribiendo `rspec` en la terminal de tu proyecto. Si todas las pruebas son exitosas, deberías ver un mensaje similar a este:

```
Finished in 0.00188 seconds (files took 0.0776 seconds to load)
1 example, 0 failures
```

Si alguna prueba falla, RSpec te mostrará información detallada sobre qué prueba falló y por qué, lo que hace que sea más fácil corregir errores en tu código.

## Profundizando

Escribir pruebas también te ayuda a pensar en tu código de una manera más estructurada y escalable. Puedes usar pruebas para guiar el diseño de tu código y asegurarte de que estás cubriendo todos los posibles casos y escenarios. También puedes usar pruebas para realizar pruebas de regresión, lo que significa que puedes asegurarte de que tus cambios no han roto ninguna funcionalidad existente.

Otra técnica útil al escribir pruebas es la prueba impulsada por el comportamiento (BDD, por sus siglas en inglés). BDD se enfoca en escribir pruebas desde la perspectiva de un usuario o cliente, en lugar de centrarse en la lógica interna del código. Esto ayuda a garantizar que tu código cumpla con los requisitos del negocio y sea fácilmente comprensible por otros miembros del equipo.

## Ver también

- [Documentación de RSpec](https://rspec.info/documentation/)
- [Tutorial de TDD con RSpec](https://semaphoreci.com/community/tutorials/getting-started-with-rspec)
- [BDD vs TDD: ¿Cuál es la diferencia?](https://www.freecodecamp.org/news/tdd-vs-bdd-c62d3b2bfd3c/)