---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:14.117278-07:00
description: "Arrays associativos, conhecidos como `std::map` ou `std::unordered_map`\
  \ em C++, preenchem a lacuna entre \xEDndices de arrays e dados do mundo real,\u2026"
lastmod: '2024-03-13T22:44:46.872755-06:00'
model: gpt-4-0125-preview
summary: "Arrays associativos, conhecidos como `std::map` ou `std::unordered_map`\
  \ em C++, preenchem a lacuna entre \xEDndices de arrays e dados do mundo real, permitindo\
  \ que voc\xEA use chaves significativas."
title: Usando arrays associativos
weight: 15
---

## Como fazer:
Em C++, os arrays associativos ganham vida com os cabeçalhos `<map>` e `<unordered_map>`. Vamos detalhar exemplos para ver ambos em ação.

### Usando `std::map`
`std::map` mantém os elementos ordenados com base na chave. Aqui está como você começa:

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ageMap;
    
    // Inserindo valores
    ageMap["Alice"] = 30;
    ageMap["Bob"] = 25;
    
    // Acessando valores
    std::cout << "Idade do Bob: " << ageMap["Bob"] << std::endl;
    
    // Iterando sobre um map
    for(const auto &pair : ageMap) {
        std::cout << pair.first << " tem " << pair.second << " anos." << std::endl;
    }
    
    return 0;
}
```

### Usando `std::unordered_map`
Quando a ordem não importa, mas o desempenho sim, `std::unordered_map` é seu amigo, oferecendo uma complexidade média mais rápida para inserções, buscas e deleções.

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> productPrice;
    
    // Inserindo valores
    productPrice["leite"] = 2.99;
    productPrice["pão"] = 1.99;
    
    // Acessando valores
    std::cout << "Preço do leite: $" << productPrice["leite"] << std::endl;
    
    // Iterando sobre um unordered_map
    for(const auto &pair : productPrice) {
        std::cout << pair.first << " custa $" << pair.second << std::endl;
    }
    
    return 0;
}
```

## Aprofundando
Arrays associativos em C++, particularmente `std::map` e `std::unordered_map`, não são apenas sobre armazenar elementos. Eles fornecem uma base para uma gestão de dados mais complexa ao permitir operações como busca, inserção e remoção em complexidades de tempo eficientes (logarítmica para `std::map` e tempo médio constante para `std::unordered_map`). Essa eficiência vem das estruturas de dados subjacentes: uma árvore balanceada para `std::map` e uma tabela de hash para `std::unordered_map`.

Historicamente, antes de serem parte da biblioteca padrão, os programadores teriam que implementar suas próprias versões ou usar bibliotecas de terceiros, levando a inconsistências e potenciais ineficiências. A inclusão dos maps na biblioteca padrão de C++ não apenas padronizou seu uso, mas também os otimizou para desempenho em diferentes compiladores e plataformas.

Embora ambos sejam poderosos, a escolha entre um `std::map` e um `std::unordered_map` depende dos detalhes do seu caso de uso. Precisa de dados ordenados e não se importa com uma pequena troca de desempenho? Opte por `std::map`. Se você está atrás de velocidade e não se importa com a ordem, `std::unordered_map` é provavelmente sua melhor aposta.

No entanto, é importante notar que ao trabalhar com estruturas de dados complexas, sempre existem trocas. Em alguns casos de nicho, outras estruturas de dados ou até mesmo bibliotecas de terceiros podem oferecer melhor desempenho ou funcionalidade adequada às suas necessidades específicas. Sempre avalie suas opções com base nos requisitos do seu projeto.
