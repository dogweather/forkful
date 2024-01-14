---
title:    "Arduino: Criando um arquivo temporário"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário com Arduino?

Criar um arquivo temporário pode ser útil em diversas situações de programação com Arduino. Alguns exemplos incluem armazenar dados temporários que serão usados posteriormente, criar um arquivo de log para rastrear erros e registros de execução, ou armazenar temporariamente informações que precisam ser acessadas rapidamente.

## Como criar um arquivo temporário com Arduino

Para criar um arquivo temporário com Arduino, você pode seguir os seguintes passos:

1. Importe a biblioteca <br> ```Arduino #include <FS.h>```
2. Inicie o sistema de arquivos <br>```Arduino SPIFFS.begin();```
3. Crie o arquivo temporário <br>```Arduino File tempFile = SPIFFS.open("/dados_temp", "w");```
4. Escreva dados no arquivo <br>```Arduino tempFile.print("Dados temporários");```
5. Feche o arquivo <br>```Arduino tempFile.close();```

Ao seguir esses passos, você criará um arquivo temporário chamado "dados_temp" na memória do sistema de arquivos do seu Arduino. Você pode personalizar o nome e o tipo do arquivo conforme suas necessidades.

## Detalhes sobre a criação de arquivos temporários

Ao criar um arquivo temporário com Arduino, é importante ter em mente que ele será armazenado na memória flash do dispositivo. Além disso, os arquivos temporários criados dessa forma serão excluídos automaticamente toda vez que o dispositivo for reiniciado.

Portanto, se você precisar manter os dados armazenados em um arquivo temporário, é necessário salvá-los em outro local antes de reiniciar o dispositivo. Você também pode usar a biblioteca <espaço reservado> para gerenciar e manipular arquivos temporários de forma mais avançada.

## Veja também

- [Documentação oficial do SPIFFS](https://arduino-esp8266.readthedocs.io)
- [Exemplo de uso de arquivos temporários com ESP8266](https://www.hackster.io)
- [Como gerenciar arquivos temporários com a biblioteca <espaço reservado>](https://github.com/exemplo)