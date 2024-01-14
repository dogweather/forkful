---
title:    "PHP: Escrevendo um arquivo de texto"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto é uma tarefa fundamental para qualquer programador PHP. Isso permite que você armazene e manipule dados de forma estruturada, criando uma comunicação eficiente entre o seu código e as informações que ele precisa processar.

## Como fazer

Para criar um arquivo de texto em PHP, você precisa seguir alguns passos simples:

1. Abra o arquivo utilizando a função `fopen()`, passando o nome do arquivo e o tipo de operação (leitura, escrita ou ambos) como argumentos.
2. Escreva o conteúdo desejado no arquivo utilizando a função `fwrite()`, especificando o ponteiro do arquivo e o texto a ser escrito.
3. Feche o arquivo utilizando a função `fclose()` para liberar recursos do sistema.

Aqui está um exemplo de código que cria um arquivo de texto chamado "texto.txt" com o conteúdo "Olá, mundo!":

```PHP
$arquivo = fopen("texto.txt", "w");
$conteudo = "Olá, mundo!";
fwrite($arquivo, $conteudo);
fclose($arquivo);
```

Ao executar esse código, você verá o arquivo "texto.txt" criado com sucesso, contendo o texto "Olá, mundo!".

## Mergulho profundo

Além dos passos básicos para escrita de um arquivo de texto, é importante mencionar alguns pontos importantes que podem ajudar a melhorar a eficiência e segurança do seu código.

Para começar, é recomendado utilizar a função `file_put_contents()` ao invés de `fopen()` e `fwrite()`, pois ela realiza todas as etapas necessárias em apenas uma chamada de função.

Também é importante sempre verificar se o arquivo foi aberto corretamente antes de efetuar a escrita, utilizando a função `is_resource()`.

Outra dica essencial é utilizar a função `file_exists()` para garantir que o arquivo que você deseja escrever já não exista, evitando assim a sobrescrita de dados importantes.

## Veja também

- Documentação oficial do PHP sobre escrita de arquivos: https://www.php.net/manual/pt_BR/function.file-put-contents.php
- Tutorial sobre manipulação de arquivos em PHP: https://www.devmedia.com.br/manipulando-arquivos-com-php/24881
- Exemplos práticos de escrita de arquivos em PHP: https://www.homehost.com.br/blog/tutoriais-e-dicas/php/como-escrever-em-arquivo-txt-com-php/