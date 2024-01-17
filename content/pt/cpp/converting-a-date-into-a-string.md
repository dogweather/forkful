---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que e por quê?

Converter uma data em uma string é um processo essencial em programação, pois permite transformar uma data em um formato legível para o usuário. É frequentemente usado em aplicativos que precisam exibir datas em diferentes idiomas ou formatos, tornando a informação mais fácil de entender e seguir.

## Como fazer:

```C++
#include <iostream>
#include <string>
#include <ctime>

using namespace std;

int main() {
   // Definir a data atual
   time_t t = time(nullptr);
   tm* timePtr = localtime(&t);
   
   // Converter a data em uma string usando o formato "MM/DD/AAAA"
   char dateStr[9];
   strftime(dateStr, 9, "%m/%d/%Y", timePtr);
   
   // Imprimir a string formatada
   cout << "Data formatada: " << dateStr << endl;
   
   return 0;
}
```

**Saída:**
```
Data formatada: 06/02/2020
```

## Mergulho Profundo:

Ao longo do tempo, diferentes linguagens de programação adotaram diferentes métodos para converter datas em strings. Enquanto o C++ usa a função `strftime()` para formatar a data, outras linguagens podem usar métodos específicos ou APIs dedicadas para isso.

Além disso, há também diferentes formatos de data em diferentes partes do mundo. Por exemplo, enquanto nos Estados Unidos as datas são frequentemente escritas no formato "MM/DD/AAAA", em outras partes do mundo é usado o formato "DD/MM/AAAA". É importante ter em mente as diferenças culturais e de formatação ao lidar com datas em um programa.

## Veja Também:

Para mais informações sobre a função `strftime()` e como formatar datas em diferentes formatos, consulte a documentação oficial do C++ em <http://www.cplusplus.com/reference/ctime/strftime/>.