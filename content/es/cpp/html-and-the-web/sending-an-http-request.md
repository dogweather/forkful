---
date: 2024-01-20 17:59:36.162588-07:00
description: "C\xF3mo hacerlo: Aqu\xED utilizaremos la biblioteca [C++ Requests](https://github.com/libcpr/cpr),\
  \ una envoltura simple y moderna para hacer solicitudes HTTP en\u2026"
lastmod: '2024-03-13T22:44:59.372257-06:00'
model: gpt-4-1106-preview
summary: "Aqu\xED utilizaremos la biblioteca [C++ Requests](https://github.com/libcpr/cpr),\
  \ una envoltura simple y moderna para hacer solicitudes HTTP en C++ inspirada por\
  \ la biblioteca Python requests."
title: Enviando una solicitud http
weight: 44
---

## Cómo hacerlo:
Aquí utilizaremos la biblioteca [C++ Requests](https://github.com/libcpr/cpr), una envoltura simple y moderna para hacer solicitudes HTTP en C++ inspirada por la biblioteca Python requests. Primero, instálala:

```bash
git clone https://github.com/libcpr/cpr.git
cd cpr
mkdir build && cd build
cmake ..
make
sudo make install
```

Ejemplo de solicitud GET:

```C++
#include <cpr/cpr.h>
#include <iostream>

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    std::cout << "Estatus: " << r.status_code << std::endl;
    std::cout << "Cuerpo: " << r.text << std::endl;
}
```

Ejemplo de solicitud POST:

```C++
#include <cpr/cpr.h>
#include <iostream>

int main() {
    cpr::Response r = cpr::Post(cpr::Url{"http://httpbin.org/post"},
                                cpr::Body{"Este es el cuerpo del mensaje"},
                                cpr::Header{{"Content-Type", "text/plain"}});
    std::cout << "Estatus: " << r.status_code << std::endl;
    std::cout << "Cuerpo: " << r.text << std::endl;
}
```

Salida para ambos, respectivamente, podría ser:

```
Estatus: 200
Cuerpo: {
  "args": {}, 
  ...
}
```

```
Estatus: 200
Cuerpo: {
  "data": "Este es el cuerpo del mensaje",
  ...
}
```

## Análisis Detallado:
Historia: El protocolo HTTP se define en el RFC 2616. Desde su aparición en 1991, ha sido fundamental en la web.

Alternativas: Aparte de `libcpr`, hay otras bibliotecas como `libcurl` y `Boost.Beast`. `libcurl` es más antigua y completa, mientras que `Boost.Beast` es parte de Boost, así que puede ser más compleja.

Detalles de implementación: Al enviar una solicitud HTTP, típicamente necesitas un verbo HTTP (GET, POST, PUT, DELETE...), una URL, encabezados opcionales y, para algunos verbos, un cuerpo de mensaje. Las respuestas contienen un código de estado (como 200 para éxito) y, normalmente, un cuerpo de mensaje.

## Ver También:
1. [C++ Requests GitHub](https://github.com/libcpr/cpr)
2. [libcurl](https://curl.se/libcurl/)
3. [Boost.Beast](https://www.boost.org/doc/libs/1_75_0/libs/beast/doc/html/index.html)
4. [RFC 2616 - Protocolo HTTP 1.1](https://www.ietf.org/rfc/rfc2616.txt)
