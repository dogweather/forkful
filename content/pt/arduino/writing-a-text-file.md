---
title:    "Arduino: Escrevendo um arquivo de texto"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Ao programar com Arduino, pode ser necessário armazenar e gravar dados em um dispositivo. Escrever um arquivo de texto é uma forma simples e eficiente de salvar informações para uso futuro. Além disso, pode ser útil para criar logs de dados ou para outros propósitos de registro.

## Como fazer?

Para escrever um arquivo de texto com Arduino, você precisará de um cartão SD e o módulo correspondente para conectá-lo ao seu Arduino. Em seguida, siga os seguintes passos:

1. Importe a biblioteca SD na sua sketch do Arduino: `#include <SD.h>`
2. Inicialize o cartão SD: `SD.begin(pin)`
3. Abra um arquivo de texto para escrita: `File myFile = SD.open("arquivo.txt", FILE_WRITE)`
4. Escreva os dados que deseja armazenar no arquivo usando o comando print ou println, por exemplo: `myFile.println("Dados importantes")`
5. Feche o arquivo: `myFile.close()`

Aqui está um exemplo de código completo que cria um arquivo chamado "myFile.txt" e escreve a frase "Olá, mundo!" nele:

```
Arduino:

#include <SD.h>

void setup() {
  // Inicialize o cartão SD
  SD.begin(10); 
  
  // Abra o arquivo para escrita
  File myFile = SD.open("myFile.txt", FILE_WRITE);

  // Escreva a frase no arquivo
  myFile.println("Olá, mundo!");

  // Feche o arquivo
  myFile.close();
}

void loop() {}
```

Ao executar este código, o arquivo "myFile.txt" será criado e a frase será escrita nele, pronta para ser acessada posteriormente.

## Mergulho profundo

Existem várias opções e funções adicionais que podem ser usadas ao escrever um arquivo de texto com Arduino, como mudar o modo de abertura do arquivo (apenas leitura ou escrita), verificar se o arquivo foi criado com sucesso, definir uma taxa de transmissão para a gravação de dados e muito mais. É importante ler a documentação completa da biblioteca SD para tirar o máximo proveito desse recurso.

## Veja também

- [Documentação da biblioteca SD](https://www.arduino.cc/en/Reference/SD)
- [Tutorial: Como escrever um arquivo de texto em um cartão SD](https://www.instructables.com/id/How-to-Write-Text-to-a-File-Using-Arduino/)
- [Vídeo tutorial: Armazenando dados em um cartão SD com Arduino](https://www.youtube.com/watch?v=4mogBvtwg2U)