---
title:                "Arduino: Verificação da existência de um diretório."
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Quando trabalhamos com desenvolvimento de projetos no Arduino, muitas vezes precisamos armazenar arquivos em um cartão SD. Porém, antes de salvar ou acessar um arquivo, é importante ter certeza de que o diretório onde queremos inseri-lo realmente existe. É essa a função que verificamos no código Arduino: garantir que o diretório existe antes de realizar qualquer operação.

## Como fazer isso?

Para verificar se um diretório existe, utilizamos a função ```SD.exists()``` seguida do nome do diretório entre parênteses. Uma possível implementação seria a seguinte:

```Arduino
if(SD.exists("meu_diretorio")){
   //Realize a operação desejada
}
```

Caso o diretório "meu_diretorio" exista, a operação será realizada. Caso contrário, nada será executado. É importante ressaltar que o nome do diretório deve ser inserido entre aspas e respeitar a estrutura de pastas do seu cartão SD.

## Aprofundando no assunto

A função ```SD.exists()``` retorna um valor booleano, ou seja, verdadeiro (true) se o diretório existir ou falso (false) se ele não existir. Além disso, é possível utilizar outras funções relacionadas, como por exemplo, a ```SD.mkdir()```, que cria um diretório caso ele não exista. Também é importante lembrar que, se existir um arquivo com o mesmo nome do diretório, a função ```SD.exists()``` irá retornar ```false```, pois o nome já está em uso.

## Veja também

- Documentação oficial da SD.h library (em inglês): https://www.arduino.cc/en/reference/SD
- Artigo sobre leitura e gravação de arquivos em cartão SD (em português): https://blog.arduino.cc/2016/07/06/how-to-control-10-objects-with-one-arduino-board-and-a-joystick/ 
- Vídeo tutorial sobre como usar um cartão SD com Arduino (em português): https://www.youtube.com/watch?v=8-V98mQd5Ok