---
title:                "PHP: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma tarefa essencial para qualquer programador PHP. Isso permite que você acesse e manipule informações armazenadas em arquivos de texto, o que é útil para criar sistemas de arquivos, gerar relatórios ou até mesmo para fins de depuração. Neste post, vamos discutir como ler um arquivo de texto em PHP e como pode ser uma habilidade valiosa para seu arsenal de programação.

## Como fazer isso em PHP

A função `file_get_contents()` é a maneira mais simples de ler um arquivo de texto em PHP. Vejamos um exemplo de como lê-lo e exibir seu conteúdo:

```
<?php
$texto = file_get_contents("arquivo.txt");
echo $texto;
```

Neste exemplo, usamos a função `file_get_contents()` para ler o conteúdo de um arquivo de texto chamado "arquivo.txt" e atribuí-lo à variável `$texto`. Em seguida, usamos a função `echo` para exibir o conteúdo do arquivo na tela. Simples, não é?

Além disso, podemos usar outras funções, como `fopen()` e `fgets()`, para ler um arquivo de texto linha por linha. Isso pode ser útil para processar arquivos maiores ou para realizar operações específicas em cada linha do texto.

## Mergulhando mais fundo

Além da função `file_get_contents()`, há várias outras funções e métodos que podemos usar para ler arquivos de texto em PHP. Por exemplo, podemos usar a classe `SplFileObject` para iterar sobre as linhas de um arquivo e realizar operações em cada uma delas.

Também é importante mencionar que existem várias opções para especificar o modo de leitura do arquivo, como "somente leitura" ou "leitura e gravação". Familiarizar-se com essas opções pode ajudá-lo a manipular os arquivos de forma mais eficiente.

Em suma, ler arquivos de texto em PHP é uma habilidade útil que pode ser aplicada em várias tarefas de programação. Com as funções e métodos certos, podemos facilmente acessar e manipular dados em arquivos de texto com facilidade.

## Veja também

- [Funções de manipulação de arquivos em PHP](https://www.php.net/manual/pt_BR/ref.filesystem.php)
- [Documentação oficial sobre a classe SplFileObject](https://www.php.net/manual/pt_BR/class.splfileobject.php)
- [Tutorial sobre como ler e escrever em arquivos de texto em PHP](https://www.devmedia.com.br/manipulando-arquivos-de-texto-com-php/4324)