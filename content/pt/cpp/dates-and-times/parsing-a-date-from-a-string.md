---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:33.674532-07:00
description: "Analisar uma data a partir de uma string envolve interpretar o formato\
  \ da string para extrair componentes da data como dia, m\xEAs e ano. Programadores\
  \ fazem\u2026"
lastmod: '2024-03-13T22:44:46.888729-06:00'
model: gpt-4-0125-preview
summary: "Analisar uma data a partir de uma string envolve interpretar o formato da\
  \ string para extrair componentes da data como dia, m\xEAs e ano. Programadores\
  \ fazem\u2026"
title: Analisando uma data a partir de uma string
---

{{< edit_this_page >}}

## O Que & Por Que?
Analisar uma data a partir de uma string envolve interpretar o formato da string para extrair componentes da data como dia, mês e ano. Programadores fazem isso para manipular entradas de usuários, ler arquivos de dados ou interagir com APIs que comunicam datas em formatos de strings. É essencial para processamento de dados, validação e execução de aritmética de datas em aplicações.

## Como Fazer:
No C++ moderno, você pode usar a biblioteca `<chrono>` para manipular datas e horas nativamente, mas ela não suporta diretamente a análise de strings sem um parsing manual para formatos mais complexos. No entanto, para formatos de data ISO 8601 e formatos customizados simples, aqui está como você pode realizar a análise.

**Usando `<chrono>` e `<sstream>`:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // Formato ISO 8601
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day data_analisada;
    iss >> std::chrono::parse("%F", data_analisada);
    
    if (!iss.fail()) {
        std::cout << "Data analisada: " << data_analisada << std::endl;
    } else {
        std::cout << "Falha ao analisar a data." << std::endl;
    }
    
    return 0;
}
```
Saída de exemplo:
```
Data analisada: 2023-04-15
```

Para formatos mais complexos ou ao lidar com versões mais antigas do C++, bibliotecas de terceiros como `date.h` (biblioteca de data de Howard Hinnant) são populares. Aqui está como você pode analisar vários formatos com ela:

**Usando a Biblioteca `date.h`:**
Certifique-se de ter a biblioteca instalada. Você pode encontrá-la [aqui](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days data_analisada;
    iss >> date::parse("%B %d, %Y", data_analisada);
    
    if (!iss.fail()) {
        std::cout << "Data analisada: " << data_analisada << std::endl;
    } else {
        std::cout << "Falha ao analisar data da string." << std::endl;
    }

    return 0;
}
```
Saída de exemplo (pode variar dependendo da localização do sistema e das configurações de data):
```
Data analisada: 2023-04-15
```
