---
aliases:
- /pt/arduino/using-associative-arrays/
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:09:58.986336-07:00
description: "No mundo do Arduino, arrays associativos permitem que voc\xEA associe\
  \ chaves a valores, algo como parear meias com seus pares. Eles s\xE3o uma escolha\
  \ acertada\u2026"
lastmod: 2024-02-18 23:08:58.403218
model: gpt-4-0125-preview
summary: "No mundo do Arduino, arrays associativos permitem que voc\xEA associe chaves\
  \ a valores, algo como parear meias com seus pares. Eles s\xE3o uma escolha acertada\u2026"
title: Usando arrays associativos
---

{{< edit_this_page >}}

## O quê & Por quê?
No mundo do Arduino, arrays associativos permitem que você associe chaves a valores, algo como parear meias com seus pares. Eles são uma escolha acertada quando você precisa armazenar e recuperar dados usando nomes descritivos, tornando seu código mais limpo e muito mais compreensível.

## Como fazer:
Estritamente falando, o Arduino não possui suporte embutido para arrays associativos como você encontraria em linguagens de nível mais alto. Mas, não tema. Podemos ser astutos usando estruturas e arrays para imitar essa funcionalidade. Aqui está um exemplo simples para criar um "array associativo" básico para armazenar e acessar temperaturas de diferentes cidades.

Primeiro, defina uma estrutura para armazenar a cidade (chave) e sua temperatura (valor):

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

Em seguida, inicialize um array de objetos `CityTemperature`:

```cpp
CityTemperature temperatures[] = {
  {"Nova Iorque", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

Veja como você pode acessar e exibir a temperatura de uma cidade específica:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("A temperatura em Los Angeles é: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // Nada aqui por enquanto.
}
```

Executando esse código, você obterá a saída:

```
A temperatura em Los Angeles é: 22.0
```

## Mergulho Profundo
Historicamente, linguagens de programação como C e C++ (das quais a sintaxe do Arduino é derivada) não vinham com arrays associativos embutidos, levando a soluções alternativas como a mostrada acima. Esta abordagem é relativamente simples, mas escala mal à medida que o tamanho dos dados aumenta devido ao seu tempo de busca O(n).

Linguagens como Python oferecem dicionários, e o JavaScript possui objetos para esse propósito, ambos muito mais eficientes para gerenciar pares chave-valor. No Arduino, quando o desempenho e a eficiência se tornam críticos, os desenvolvedores podem optar por estruturas de dados mais especializadas, como tabelas de hash, implementadas via bibliotecas.

Embora o Arduino não suporte nativamente arrays associativos, a comunidade desenvolveu bibliotecas como `HashMap` que podem ser adicionadas ao seu projeto para fornecer funcionalidades semelhantes com desempenho melhor do que uma abordagem faça você mesmo. Estas bibliotecas normalmente oferecem meios mais elegantes e eficientes de gerenciar arrays associativos, especialmente para projetos mais complexos.
