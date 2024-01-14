---
title:    "Arduino: Verificando se um diretório existe"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que 

Se você está familiarizado com a programação Arduino, provavelmente já se deparou com a necessidade de verificar se um diretório existe em seu projeto. Isso pode ser importante para garantir que seu código funcione corretamente e evitar possíveis erros. Neste artigo, vamos explorar como verificar a existência de um diretório em seu código Arduino.

## Como fazer

Para verificar se um diretório existe, precisamos usar a função `SD.exists()` da biblioteca SD. Esta função recebe como parâmetro o nome do diretório que desejamos verificar e retorna `true` se o diretório existir ou `false` caso contrário.

Vamos ver um exemplo de código para entender melhor:

```Arduino
#include <SD.h>

// Definir o pino do cartão SD
const int chipSelect = 10;

void setup() {
  // Inicializar o cartão SD
  SD.begin(chipSelect);

  // Verificar se o diretório "logs" existe
  if (SD.exists("/logs")) {
    Serial.println("O diretório existe!");
  } else {
    Serial.println("O diretório não existe!");
  }
}

void loop() {
  // Seu código aqui
}
```

No exemplo acima, estamos usando o `SD.begin()` para inicializar o cartão SD e, em seguida, verificamos se o diretório "logs" existe usando a função `SD.exists()`. Caso o diretório exista, imprimimos uma mensagem na porta serial informando que ele existe.

Você pode adaptar esse código para suas próprias necessidades, alterando o nome do diretório ou adicionando outras verificações. Lembre-se de incluir a biblioteca SD no início do seu código.

## Mergulho profundo

Para realmente entender como o Arduino verifica se um diretório existe, precisamos ter um conhecimento básico de como os cartões SD funcionam. Um cartão SD é dividido em setores, que por sua vez são divididos em blocos. Cada bloco contém 512 bytes de informações e eles são acessados através de seus endereços lógicos.

A função `SD.exists()` usa o commando `CMD13` para verificar se o diretório existe. Esse comando envia um endereço lógico para o controlador do cartão SD e espera por uma resposta. Se o diretório existir, o controlador do cartão SD retornará uma resposta positiva, caso contrário, ele retornará uma resposta negativa.

É importante notar que a função `SD.exists()` só verifica a existência de um diretório, não garante que ele esteja vazio. Portanto, se você quiser garantir que um diretório esteja vazio, precisará adicionar mais verificações ao seu código.

## Veja também

- [Documentação oficial da biblioteca SD](https://www.arduino.cc/en/Reference/SD)
- [Tutorial de uso do cartão SD](https://blog.arduino.cc/2019/01/22/sd-card-shields-best-tips-for-the-best-uses/)