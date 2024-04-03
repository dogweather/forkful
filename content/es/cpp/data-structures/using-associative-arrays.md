---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:17.968607-07:00
description: "Los arreglos asociativos, conocidos como `std::map` o `std::unordered_map`\
  \ en C++, llenan el vac\xEDo entre los \xEDndices de los arreglos y los datos del\
  \ mundo\u2026"
lastmod: '2024-03-13T22:44:59.368436-06:00'
model: gpt-4-0125-preview
summary: "Los arreglos asociativos, conocidos como `std::map` o `std::unordered_map`\
  \ en C++, llenan el vac\xEDo entre los \xEDndices de los arreglos y los datos del\
  \ mundo real, permiti\xE9ndote usar claves significativas."
title: Uso de matrices asociativas
weight: 15
---

## ¿Qué y Por Qué?

Los arreglos asociativos, conocidos como `std::map` o `std::unordered_map` en C++, llenan el vacío entre los índices de los arreglos y los datos del mundo real, permitiéndote usar claves significativas. Son la opción preferente cuando necesitas búsquedas, inserciones y eliminaciones rápidas usando claves en lugar de posiciones de índice.

## Cómo utilizarlos:

En C++, los arreglos asociativos cobran vida con los encabezados `<map>` y `<unordered_map>`. Desglosemos ejemplos para ver ambos en acción.

### Usando `std::map`

`std::map` mantiene los elementos ordenados basados en la clave. Así es como puedes comenzar:

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ageMap;
    
    // Insertando valores
    ageMap["Alice"] = 30;
    ageMap["Bob"] = 25;
    
    // Accediendo a los valores
    std::cout << "Edad de Bob: " << ageMap["Bob"] << std::endl;
    
    // Iterando sobre un map
    for(const auto &par : ageMap) {
        std::cout << par.first << " tiene " << par.second << " años." << std::endl;
    }
    
    return 0;
}
```

### Usando `std::unordered_map`

Cuando el orden no importa, pero sí el rendimiento, `std::unordered_map` es tu amigo, ofreciendo una complejidad promedio más rápida para inserciones, búsquedas y eliminaciones.

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> productPrice;
    
    // Insertando valores
    productPrice["leche"] = 2.99;
    productPrice["pan"] = 1.99;
    
    // Accediendo a los valores
    std::cout << "Precio de la leche: $" << productPrice["leche"] << std::endl;
    
    // Iterando sobre un unordered_map
    for(const auto &par : productPrice) {
        std::cout << par.first << " cuesta $" << par.second << std::endl;
    }
    
    return 0;
}
```

## Inmersión Profunda

Los arreglos asociativos en C++, particularmente `std::map` y `std::unordered_map`, no son solo acerca de almacenar elementos. Proporcionan una base para una gestión de datos más compleja al permitir operaciones como buscar, insertar y eliminar en complejidades de tiempo eficientes (logarítmica para `std::map` y en promedio tiempo constante para `std::unordered_map`). Esta eficiencia proviene de las estructuras de datos subyacentes: un árbol equilibrado para `std::map` y una tabla hash para `std::unordered_map`.

Históricamente, antes de que estos fueran parte de la biblioteca estándar, los programadores tenían que implementar sus propias versiones o usar bibliotecas de terceros, lo que llevaba a inconsistencias y posibles ineficiencias. La inclusión de mapas en la biblioteca estándar de C++ no solo estandarizó su uso, sino que también los optimizó para el rendimiento en diferentes compiladores y plataformas.

Aunque ambos son poderosos, la elección entre un `std::map` y un `std::unordered_map` depende de los detalles de tu caso de uso. ¿Necesitas datos ordenados y no te importa un ligero compromiso en el rendimiento? Opta por `std::map`. Si buscas velocidad y el orden no te importa, `std::unordered_map` es probablemente tu mejor opción.

Sin embargo, es importante destacar que cuando se trabaja con estructuras de datos complejas, siempre hay compromisos. En algunos casos particulares, otras estructuras de datos o incluso bibliotecas de terceros podrían ofrecer un rendimiento mejor o funcionalidades más adecuadas para tus necesidades específicas. Siempre evalúa tus opciones basándote en los requisitos de tu proyecto.
