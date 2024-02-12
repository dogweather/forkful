---
title:                "Uso de matrices asociativas"
aliases:
- es/ruby/using-associative-arrays.md
date:                  2024-01-30T19:12:26.302449-07:00
model:                 gpt-4-0125-preview
simple_title:         "Uso de matrices asociativas"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Los arreglos asociativos, más conocidos como hashes en Ruby, permiten emparejar claves únicas con valores. Son indispensables cuando necesitas llevar un registro de elementos a través de una referencia específica, como almacenar las propiedades de un objeto o acceder rápidamente a datos mediante un identificador único.

## Cómo hacerlo:

Crear y usar hashes en Ruby es sencillo. Puedes inicializar un hash vacío, llenarlo con pares clave-valor, acceder a los valores por sus claves y más. Así es cómo se hace:

```Ruby
# Creando un hash
my_hash = { "name" => "John Doe", "age" => 30 }

# Otra manera de crear un hash
another_hash = Hash.new
another_hash["position"] = "Desarrollador"

# Accediendo los valores del hash
puts my_hash["name"] # Salida: John Doe

# Añadiendo un nuevo par clave-valor
my_hash["language"] = "Ruby"
puts my_hash # Salida: {"name"=>"John Doe", "age"=>30, "language"=>"Ruby"}

# Iterando a través de un hash
my_hash.each do |key, value|
  puts "#{key}: #{value}"
end
# Salida:
# name: John Doe
# age: 30
# language: Ruby
```

También puedes usar símbolos como claves más eficientes:

```Ruby
# Usando símbolos para las claves
symbol_hash = { name: "Jane Doe", age: 22 }
puts symbol_hash[:name] # Salida: Jane Doe
```

## Estudio Profundo:

El concepto de arreglos asociativos no es único de Ruby; muchos lenguajes los implementan bajo varios nombres, como diccionarios en Python u objetos en JavaScript (cuando se usan como pares clave-valor). En las etapas iniciales de Ruby, los hashes eran algo más lentos y no tan versátiles. Sin embargo, con el tiempo, la implementación de hashes en Ruby se ha optimizado mucho, especialmente para claves de símbolos, haciéndolos extremadamente eficientes para accesos y actualizaciones frecuentes.

Los hashes de Ruby se destacan por su facilidad de uso sintáctico y flexibilidad: puedes usar casi cualquier tipo de objeto como clave, aunque los símbolos y las cadenas son los más comunes. Internamente, los hashes de Ruby están implementados usando un algoritmo de hashing que equilibra la velocidad y la eficiencia de memoria, incluso a medida que el número de elementos escala.

Aunque los hashes son increíblemente versátiles, no son la solución total para el almacenamiento de datos en Ruby. Para colecciones ordenadas, los arreglos son más apropiados, y para conjuntos de elementos únicos, un Set podría ser una mejor opción. Además, para estructuras de datos muy complejas, podría ser aconsejable crear clases personalizadas.

Recuerda, la elección de usar un hash frente a otras estructuras de datos depende en gran medida del caso de uso específico: los hashes sobresalen en búsquedas rápidas y en mantener asociaciones entre claves únicas y sus valores.
