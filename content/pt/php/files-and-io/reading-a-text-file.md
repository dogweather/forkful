---
date: 2024-01-20 17:54:46.605404-07:00
description: "Ler um arquivo de texto em PHP \xE9 basicamente extrair o conte\xFA\
  do de um arquivo armazenado em seu sistema para utiliz\xE1-lo no seu c\xF3digo.\
  \ Programadores fazem\u2026"
lastmod: '2024-03-13T22:44:46.684513-06:00'
model: gpt-4-1106-preview
summary: "Ler um arquivo de texto em PHP \xE9 basicamente extrair o conte\xFAdo de\
  \ um arquivo armazenado em seu sistema para utiliz\xE1-lo no seu c\xF3digo. Programadores\
  \ fazem\u2026"
title: Lendo um arquivo de texto
weight: 22
---

## What & Why?
Ler um arquivo de texto em PHP é basicamente extrair o conteúdo de um arquivo armazenado em seu sistema para utilizá-lo no seu código. Programadores fazem isso para acessar dados, configurar aplicações ou manipular informações sem a necessidade de digitar tudo manualmente.

## How to:
Vamos direto ao ponto com um código e seu resultado:

```PHP
<?php
// Abre o arquivo no modo de leitura ('r')
$arquivo = fopen("exemplo.txt", "r");

// Verifica se o arquivo foi aberto com sucesso
if ($arquivo) {
    // Lê o arquivo linha por linha
    while (($linha = fgets($arquivo)) !== false) {
        echo $linha;
    }

    // Fecha o arquivo
    fclose($arquivo);
} else {
    // Mensagem de erro caso não consiga abrir o arquivo
    echo "Erro ao abrir o arquivo.";
}
?>
```
Saída:
```
Primeira linha do texto.
Segunda linha do texto.
Terceira linha do texto.
```

## Deep Dive
Ler arquivos de texto é uma função básica mas essencial. Em PHP, as coisas começaram simples, com funções como `fopen()`, `fgets()`, e `fclose()`. Ao longo dos anos, o PHP evoluiu e agora também oferece funções mais sofisticadas como `file_get_contents()` e `file()` que leem todo o arquivo de uma vez só, mas às vezes você só precisa ler linha por linha, o que economiza memória.

Alternativas incluem o uso de `SplFileObject` do Standard PHP Library (SPL), que proporciona uma orientação a objetos para leitura de arquivos. Outro detalhe técnico é o tratamento de erros: é uma boa prática verificar se realmente você conseguiu abrir o arquivo antes de tentar ler - isso evita mensagens de erro feias e experiências ruins para o usuário.

## See Also
Aqui estão alguns recursos para expandir seu conhecimento:

- Documentação oficial do PHP sobre manipulação de arquivos: https://www.php.net/manual/pt_BR/book.filesystem.php
- Um artigo sobre SPL e `SplFileObject`: https://www.php.net/manual/pt_BR/class.splfileobject.php
- Exemplos e tutoriais sobre tratamento de erros em PHP: https://www.php.net/manual/pt_BR/function.error-reporting.php
