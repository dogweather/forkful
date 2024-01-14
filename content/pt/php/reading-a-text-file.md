---
title:                "PHP: Lendo um arquivo de texto"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em PHP?

Ler um arquivo de texto em PHP é uma tarefa comum para aqueles que trabalham com linguagem de programação. Isso permite que você acesse e manipule o conteúdo de um arquivo, o que é essencial para muitos aplicativos e projetos. Neste post, vamos discutir como você pode facilmente ler e processar arquivos de texto usando o PHP.

## Como ler um arquivo de texto em PHP

Para ler um arquivo de texto em PHP, você precisará seguir os seguintes passos:

1. Primeiramente, você precisará abrir o arquivo usando a função `fopen()`, que aceita dois parâmetros: o nome do arquivo e o modo de leitura. Por exemplo, para abrir um arquivo chamado "exemplo.txt" em modo de leitura, você usaria `fopen("exemplo.txt", "r");`.

2. Em seguida, você precisará usar a função `fgets()` para ler cada linha do arquivo. Você pode armazenar o retorno dessa função em uma variável ou usá-lo diretamente em um loop para ler todas as linhas do arquivo.

3. Depois de terminar de ler o arquivo, certifique-se de fechá-lo usando a função `fclose()`.

Aqui está um exemplo de código que lê um arquivo de texto chamado "exemplo.txt" e imprime cada linha:

```PHP
<?php
$file = fopen("exemplo.txt", "r");

// Loop para ler cada linha do arquivo
while (!feof($file)) {
  $line = fgets($file);
  echo $line;
}

fclose($file);
?>
```

Exemplo de saída:

```
Este é o primeiro exemplo de linha.
Esta é a segunda linha.
Esta é a terceira linha.
```

## Aprofundando-se na leitura de arquivos de texto em PHP

Para aprofundar seu conhecimento sobre a leitura de arquivos de texto em PHP, aqui estão algumas informações adicionais que podem ser úteis:

- Você também pode usar a função `file()` para ler todo o conteúdo de um arquivo em um array, onde cada elemento do array é uma linha do arquivo.
- Para ler um arquivo e armazenar seu conteúdo em uma variável, você pode usar a função `file_get_contents()`.
- Ao usar a função `fgets()`, você pode definir o segundo parâmetro para especificar o número de caracteres que deseja ler de cada linha.

Com essas informações, você pode ler e manipular arquivos de texto de várias maneiras em seu código PHP.

## Veja também

- [Documentação PHP - Leitura de arquivos](https://www.php.net/manual/pt_BR/function.fopen.php)
- [W3Schools - Leitura de arquivos em PHP](https://www.w3schools.com/php/php_file_open.asp)
- [PHPJabbers - Ler e escrever em arquivos com PHP](https://www.phpjabbers.com/ler-e-escrever-em-arquivos-usando-php-php64.html)

Com este conhecimento, você estará pronto para ler arquivos de texto em PHP e utilizá-los em suas aplicações de forma eficiente. Esperamos que este post tenha sido útil para você!