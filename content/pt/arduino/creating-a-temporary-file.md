---
title:    "Arduino: Criando um arquivo temporário"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Por que criar um arquivo temporário é útil para o seu projeto Arduino?

Criar um arquivo temporário pode ser extremamente benéfico para o desenvolvimento do seu projeto Arduino. Esses arquivos temporários são úteis quando você precisa armazenar dados temporariamente durante a execução do seu código. Eles também podem ser usados para armazenar informações temporárias, como registros ou dados de configuração.

## Como criar um arquivo temporário no seu projeto Arduino

Para criar um arquivo temporário no seu projeto Arduino, você precisará seguir alguns passos simples. Primeiramente, é necessário incluir a biblioteca "SD.h" no seu código. Essa biblioteca permite acessar o cartão SD do seu Arduino.

Em seguida, crie uma variável com o tipo de dados "File" e dê um nome para o seu arquivo temporário. Por exemplo: "tempFile".

Dentro da função "setup", você pode usar o comando "SD.begin()" para inicializar o cartão SD. Depois, use o comando "tempFile = SD.open(nome do arquivo, opção de abertura)" para criar o seu arquivo temporário. Por exemplo: "tempFile = SD.open("temp.txt", FILE_WRITE)".

Agora, dentro da função "loop", você pode usar o comando "tempFile.println()" para escrever dados no seu arquivo temporário. E se você quiser ler os dados do arquivo, use "tempFile.read()". Lembre-se de sempre fechar o arquivo após o uso, usando "tempFile.close()" para evitar problemas de memória.

Aqui está um exemplo completo de código para criação de um arquivo temporário e escrita de dados nele:

```
#include <SD.h>

File tempFile;

void setup(){
  SD.begin();
  tempFile = SD.open("temp.txt", FILE_WRITE);
  tempFile.println("Dados temporários do projeto Arduino");
  tempFile.close();
}

void loop(){
  //código da sua aplicação
}
```

## Deep Dive: Mais informações sobre a criação de arquivos temporários

Para garantir que seu arquivo temporário seja criado com sucesso, é importante seguir algumas boas práticas. Uma delas é sempre verificar se o cartão SD está presente antes de tentar criar o arquivo. Isso pode ser feito usando o comando "SD.exists()" e passando o nome do seu arquivo como parâmetro.

Além disso, é importante escolher um nome de arquivo único e relevante para evitar problemas de sobreposição de arquivos. Você também pode usar uma variável de tempo no nome do arquivo para garantir sua unicidade.

Seu cartão SD deve ter espaço suficiente para a criação do arquivo temporário. Portanto, é importante monitorar isso e garantir que você tenha espaço disponível suficiente antes de criar o arquivo.

## Veja também

- [Documentação da biblioteca SD.h](https://www.arduino.cc/en/Reference/SD)
- [Exemplo de código para criação de arquivo temporário com cartão SD](https://www.arduino.cc/en/Tutorial/LibraryExamples/SDReadWrite)

Espero que este artigo tenha sido útil para você entender como criar um arquivo temporário no seu projeto Arduino. Com essa dica, você pode armazenar temporariamente dados e informações importantes para o bom funcionamento da sua aplicação. Experimente e veja como isso pode facilitar o desenvolvimento do seu projeto.