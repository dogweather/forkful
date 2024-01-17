---
title:                "Escrevendo em erro padrão."
html_title:           "Bash: Escrevendo em erro padrão."
simple_title:         "Escrevendo em erro padrão."
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que & Porquê?

A escrita para o erro padrão, também conhecida como stderr, é uma forma de direcionar mensagens de erro ou saída de um programa para o usuário. Isso é especialmente útil quando se trabalha com scripts de Bash, pois permite uma melhor depuração de erros e torna o processo de desenvolvimento mais eficiente.

## Como fazer:

Para escrever para o stderr em Bash, basta usar o número 2 antes do sinal de maior (>). Por exemplo:

```
echo "Mensagem de erro" >&2
```

Isso enviará a mensagem de erro para o stderr em vez do stdout (saída padrão). Como resultado, a mensagem será exibida na tela, permitindo que você veja e lide com o erro de forma adequada.

## Profundidade

A prática de escrever para o stderr remonta aos primórdios da programação de computadores, quando os desenvolvedores precisavam encontrar uma maneira de lidar com erros e avisos em seus programas. Alguns programadores optam por escrever para o stderr em vez de imprimir mensagens de erro formatadas, pois isso pode fornecer informações mais precisas e detalhadas sobre o erro.

No entanto, existem alternativas para escrever para o stderr, como usar o comando "exit" com um código de erro específico ou tratamento de erros em linguagens de programação mais avançadas, como Python ou Java.

Em termos de implementação, a escrita para o stderr em Bash é relativamente simples e pode ser facilmente implementada em scripts e programas. É importante lembrar que, ao usar essa técnica, é necessário gerenciar corretamente as mensagens de erro para fornecer informações úteis ao usuário e melhorar a experiência do usuário.

## Veja também:

Para obter mais informações sobre como escrever para o stderr em Bash, confira a documentação oficial do Bash ou outros guias online sobre a prática. Além disso, você também pode explorar as diferentes alternativas e técnicas para lidar com erros em programas.