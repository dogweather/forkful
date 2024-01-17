---
title:                "Escribiendo pruebas"
html_title:           "Ruby: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Escribir pruebas (tests) es un proceso en el que los programadores crean pequeños bloques de código para verificar si su código principal funciona correctamente. Esto les ayuda a detectar y solucionar errores antes de que su código se implemente en producción. 

## ¿Cómo hacerlo?
A continuación se muestran dos ejemplos de pruebas escritas con Ruby. Estos ejemplos asumen que ya tienes el entorno de desarrollo de Ruby y la gema (gem) RSpec instalados.

```
# Ejemplo 1: Prueba de suma
require 'rspec/autorun'
 
RSpec.describe "Suma" do
  it "debe sumar dos números correctamente" do
    expect(1+2).to eq(3)
  end
end
```

```
# Ejemplo 2: Prueba de división
require 'rspec/autorun'
 
RSpec.describe "División" do
  it "debe dividir dos números correctamente" do
    expect(10/2).to eq(5)
  end
end
```

La salida de estas pruebas debe ser ```0 failures``` si se ejecutan correctamente.

## Profundizando
Las pruebas se han vuelto una parte fundamental del desarrollo de software en la actualidad. Detectar y solucionar errores desde el principio del proceso ahorra tiempo y dinero a largo plazo. 

Existen otras herramientas para escribir pruebas en Ruby, como Minitest y Test::Unit. Además, también se pueden escribir pruebas para otros lenguajes de programación, como Java y Python. 

Para implementar pruebas en un proyecto, se recomienda seguir un enfoque conocido como TDD (Test-Driven Development). Este consiste en escribir las pruebas primero y luego el código que las cumpla. 

## Ver también
- [RSpec documentation](https://rspec.info/)
- [Minitest documentation](https://github.com/seattlerb/minitest)
- [Test::Unit documentation](https://apidock.com/ruby/Test/Unit)