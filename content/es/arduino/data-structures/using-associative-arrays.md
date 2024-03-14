---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:09:46.506280-07:00
description: "En el mundo de Arduino, los arrays asociativos te permiten emparejar\
  \ claves con valores, algo as\xED como emparejar\xEDas calcetines con sus pares.\
  \ Son una\u2026"
lastmod: '2024-03-13T22:44:59.327265-06:00'
model: gpt-4-0125-preview
summary: "En el mundo de Arduino, los arrays asociativos te permiten emparejar claves\
  \ con valores, algo as\xED como emparejar\xEDas calcetines con sus pares. Son una\u2026"
title: Uso de matrices asociativas
---

{{< edit_this_page >}}

## ¿Qué y por qué?
En el mundo de Arduino, los arrays asociativos te permiten emparejar claves con valores, algo así como emparejarías calcetines con sus pares. Son una opción a la que recurrir cuando necesitas almacenar y recuperar datos utilizando nombres descriptivos, haciendo que tu código sea más limpio y mucho más comprensible.

## Cómo hacerlo:
Estrictamente hablando, Arduino no tiene soporte incorporado para arrays asociativos como los encontrarías en lenguajes de programación de más alto nivel. Pero, no temas. Podemos ingeniárnoslas usando estructuras y arrays para imitar esta funcionalidad. Aquí tienes un ejemplo simple para crear un "array asociativo" básico para almacenar y acceder a las temperaturas de diferentes ciudades.

Primero, define una estructura para contener la ciudad (clave) y su temperatura (valor):

```cpp
struct CiudadTemperatura {
  String ciudad;
  float temperatura;
};
```

A continuación, inicializa un array de objetos `CiudadTemperatura`:

```cpp
CiudadTemperatura temperaturas[] = {
  {"Nueva York", 19.5},
  {"Los Ángeles", 22.0},
  {"Chicago", 17.0}
};
```

Así es como puedes acceder y mostrar la temperatura de una ciudad específica:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperaturas[i].ciudad == "Los Ángeles") {
      Serial.print("La temperatura en Los Ángeles es: ");
      Serial.println(temperaturas[i].temperatura);
    }
  }
}

void loop() {
  // Nada aquí por ahora.
}
```

Ejecutar este código te daría la salida:

```
La temperatura en Los Ángeles es: 22.0
```

## Estudio Profundo
Históricamente, lenguajes de programación como C y C++ (de los cuales se deriva la sintaxis de Arduino) no venían con arrays asociativos incorporados, lo que llevó a soluciones alternativas como la mostrada arriba. Este enfoque es relativamente simple pero escala mal a medida que el tamaño de los datos aumenta debido a su tiempo de búsqueda O(n).

Lenguajes como Python ofrecen diccionarios, y JavaScript tiene objetos para este propósito, ambos son mucho más eficientes para administrar pares clave-valor. En Arduino, cuando el rendimiento y la eficiencia se vuelven críticos, los desarrolladores podrían optar por estructuras de datos más especializadas, como tablas hash, implementadas a través de bibliotecas.

Aunque Arduino no admite de manera nativa arrays asociativos, la comunidad ha desarrollado bibliotecas como `HashMap` que se pueden agregar a tu proyecto para proporcionar una funcionalidad similar con un mejor rendimiento que un enfoque hecho por ti mismo. Estas bibliotecas ofrecen generalmente un medio más elegante y eficiente de gestionar arrays asociativos, especialmente para proyectos más complejos.
