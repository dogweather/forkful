---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:49.626713-07:00
description: "Como fazer: O Arduino n\xE3o tem suporte integrado para regex diretamente\
  \ em sua biblioteca padr\xE3o. No entanto, voc\xEA pode alcan\xE7ar funcionalidade\
  \ semelhante\u2026"
lastmod: '2024-03-13T22:44:46.829405-06:00'
model: gpt-4-0125-preview
summary: "O Arduino n\xE3o tem suporte integrado para regex diretamente em sua biblioteca\
  \ padr\xE3o."
title: "Usando express\xF5es regulares"
weight: 11
---

## Como fazer:
O Arduino não tem suporte integrado para regex diretamente em sua biblioteca padrão. No entanto, você pode alcançar funcionalidade semelhante a regex para padrões simples usando funções básicas de string, ou para necessidades mais complexas, integrar uma biblioteca de terceiros como `regex`.

### Correspondência Básica de Strings sem Regex
Para necessidades básicas, como encontrar uma substring, você pode usar a função `String.indexOf()`:
```cpp
String data = "Valor do sensor: 12345";
int index = data.indexOf("valor:");
if (index != -1) {
  String valor = data.substring(index + 6).trim();
  Serial.println(valor); // Saída: 12345
}
```

### Usando uma Biblioteca de Terceiros para Regex
Para lidar com padrões mais complexos, você pode considerar uma biblioteca como `regex`. Após instalar a biblioteca, você pode usá-la da seguinte maneira:

1. **Instalação**: A biblioteca `regex` pode não estar prontamente disponível no Gerenciador de Bibliotecas do Arduino, então talvez seja necessário instalá-la manualmente baixando de uma fonte confiável e adicionando-a à sua pasta de bibliotecas do Arduino.

2. **Exemplo de Uso**:
Assumindo que a biblioteca forneça funcionalidades semelhantes às implementações padrão de regex, você poderia usá-la da seguinte forma:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // Aguarda o Serial estar pronto
  
  regex_t reg;
  const char* padrão = "[0-9]+"; // Corresponde a uma sequência de dígitos
  regcomp(&reg, padrão, REG_EXTENDED);
  
  const char* test_str = "Valor do sensor: 12345";
  
  regmatch_t correspondências[1];
  if (regexec(&reg, test_str, 1, correspondências, 0) == 0) {
    // Extrai e imprime a parte que corresponde
    int inicio = correspondências[0].rm_so;
    int fim = correspondências[0].rm_eo;
    char correspondência[fim-inicio+1];
    strncpy(correspondência, test_str + inicio, fim-inicio);
    correspondência[fim-inicio] = '\0';
    
    Serial.print("Correspondência encontrada: ");
    Serial.println(correspondência); // Saída: 12345
  } else {
    Serial.println("Nenhuma correspondência encontrada");
  }
  
  regfree(&reg); // Libera a memória alocada para regex
}

void loop() {
  // coloque seu código principal aqui, para executar repetidamente:
}
```

**Nota**: A sintaxe e funções específicas usadas aqui são para fins ilustrativos e podem variar com base nos detalhes da implementação real da biblioteca `regex` que você escolher. Sempre consulte a documentação da biblioteca para obter informações precisas e atualizadas.
