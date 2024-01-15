---
title:                "Criando um arquivo temporário"
html_title:           "Arduino: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário com o Arduino?

Criar um arquivo temporário com o Arduino pode ser útil em diversas situações, como armazenar dados temporários ou criar backups em caso de falhas no sistema. Além disso, pode ser uma maneira eficiente de gerenciar os recursos de memória do dispositivo.

## Como fazer?

Para criar um arquivo temporário com o Arduino, siga os seguintes passos:

1. Inicie o código com a biblioteca padrão <SPI.h> e a biblioteca específica <SD.h> para gerenciar o cartão SD.
    ```Arduino
    #include <SPI.h>
    #include <SD.h>
    ```
2. Defina uma variável para armazenar o nome do arquivo e outra para o conteúdo que será gravado no arquivo.
    ```Arduino
    String fileName = "arquivo_temp.txt";
    String content = "Conteúdo do arquivo temporário";
    ```
3. Inicialize o cartão SD com a função SD.begin().
    ```Arduino
    if(!SD.begin(4)){
        Serial.println("Falha ao iniciar o cartão SD.");
        return;
    }
    ```
4. Abra um novo arquivo com a função SD.open() e utilize o modo de gravação "FILE_WRITE" para criar um arquivo novo ou sobrescrever um existente.
    ```Arduino
    File file = SD.open(fileName, FILE_WRITE);
    ```
5. Escreva o conteúdo no arquivo utilizando a função file.println().
    ```Arduino
    file.println(content);
    ```
6. Feche o arquivo com a função file.close() para garantir que os dados sejam gravados corretamente.
    ```Arduino
    file.close();
    ```

## Mergulho profundo

Ao criar um arquivo temporário no Arduino, é importante ter alguns cuidados para garantir o bom funcionamento do sistema. Algumas dicas importantes são:

- Certifique-se de fechar o arquivo após escrever o conteúdo para evitar a perda de dados.
- É recomendável utilizar a função file.flush() para forçar a gravação imediata dos dados no cartão SD.
- Sempre verifique se o cartão SD está devidamente conectado e funcionando antes de iniciar a leitura ou escrita de arquivos.
- Gerencie a memória do dispositivo corretamente, evitando a criação de muitos arquivos temporários para evitar problemas de armazenamento.

## Veja também
- [Documentação da biblioteca SD.h](https://www.arduino.cc/en/Reference/SD)
- [Tutorial completo sobre uso de arquivos com o Arduino](https://www.arduino.cc/en/Tutorial/Files)
- [Fórum oficial do Arduino](https://forum.arduino.cc/index.php)