---
title:    "Arduino: Buscando e Substituindo Texto"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Se você é um entusiasta da programação e adora desbravar novos projetos usando a placa Arduino, provavelmente já se deparou com a necessidade de fazer substituições de texto em seus códigos. A técnica de busca e substituição é extremamente útil para fazer alterações rápidas e eficientes em seus programas.

## Como fazer

Para fazer uma busca e substituição de texto no seu código Arduino, siga os seguintes passos:

1. Abra o código que deseja modificar em sua plataforma de desenvolvimento Arduino.
2. Localize a função ```replace()```, que é responsável por fazer as substituições.
3. Dentro dos parênteses da função, coloque o texto que deseja substituir entre aspas simples, seguido do texto que irá substituí-lo entre aspas duplas.

Por exemplo, se quisermos substituir a palavra "maçã" por "banana" em nosso código, devemos utilizar a função da seguinte forma: ```replace('maçã', 'banana');```

Agora é só salvar seu código e realizar o upload para a placa Arduino.

## Mergulho profundo

Além da função ```replace()```, existem outras ferramentas que podem ser utilizadas para fazer busca e substituição de texto em códigos Arduino. Algumas das mais comuns são:

- A função ```replaceAll()```, que substitui todas as ocorrências de uma palavra por outra;
- O uso de expressões regulares, que permitem buscar e substituir padrões em textos;
- O uso de bibliotecas específicas, como a "StringReplace" ou "String Functions", que oferecem funções mais avançadas para busca e substituição de texto.

É importante lembrar que a busca e substituição de texto deve ser utilizada com cuidado, pois alterações indevidas podem causar erros no código. Faça sempre um backup do seu código antes de realizar qualquer modificação.

## Veja também

- [Documentação oficial do Arduino sobre a função replace()](https://www.arduino.cc/reference/en/language/functions/communication/serial/replace/)
- [Exemplo de uso da função replace() em um projeto Arduino](https://create.arduino.cc/projecthub/ukioex/replace-using-function-6707f1?ref=search&ref_id=replace&offset=1)
- [Vídeo tutorial sobre busca e substituição de texto em códigos Arduino](https://www.youtube.com/watch?v=8o9JQgOsxt4)