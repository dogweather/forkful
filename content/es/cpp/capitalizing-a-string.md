---
title:                "Capitalizando una cadena de texto"
html_title:           "C++: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Aventura en C++: Capitalizando Cadenas de Texto

## ¿Qué y por qué?
Capitalizar una cadena implica convertir todas sus letras iniciales en mayúsculas. Es útil para los programadores cuando necesitan estandarizar la entrada o mejorar la legibilidad.

## Cómo se hace:
Puedes capitalizar una cadena en C++ de la siguiente manera:

```C++
#include <algorithm>
#include <string>

std::string str = "hola mundo";

std::transform(str.begin(), str.end(), str.begin(), ::toupper);

std::cout << str;
```

La salida de este código será:

```
HOLA MUNDO
```

## Un vistazo más profundo:
Historia: originalmente, esto se usaba en lenguajes primitivos que no distinguen entre mayúsculas y minúsculas.

Alternativas: En lugar de `std::transform`, también puedes usar funciones `for` y `toupper`:

```C++
std::string str = "hola mundo";
for(auto & c : str) c = toupper(c);

std::cout << str;
```

Detalles de la implementación: `std::transform` aplica 'toupper' a cada caracter de la cadena. La función `toupper` se encuentra en la librería `<cctype>`.

## Ver también:
Para más información, consulta los siguientes enlaces:

- Documentación de std::transform: http://www.cplusplus.com/reference/algorithm/transform/
- Post de Stack Overflow sobre la capitalización de cadenas: https://stackoverflow.com/questions/735204/convert-a-string-in-c-to-upper-case
- Documentación de std::toupper: https://en.cppreference.com/w/cpp/string/byte/toupper