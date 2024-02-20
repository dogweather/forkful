---
date: 2024-01-20 17:58:23.496288-07:00
description: "Procurar e substituir texto em PHP \xE9 como mudar uma palavra espec\xED\
  fica em um livro inteiro. Programadores fazem isso para atualizar dados, corrigir\
  \ erros\u2026"
lastmod: 2024-02-19 22:05:05.700271
model: gpt-4-1106-preview
summary: "Procurar e substituir texto em PHP \xE9 como mudar uma palavra espec\xED\
  fica em um livro inteiro. Programadores fazem isso para atualizar dados, corrigir\
  \ erros\u2026"
title: Pesquisando e substituindo texto
---

{{< edit_this_page >}}

## O Que & Porquê?
Procurar e substituir texto em PHP é como mudar uma palavra específica em um livro inteiro. Programadores fazem isso para atualizar dados, corrigir erros ou alterar a informação rapidamente sem ter que procurar manualmente por cada ocorrência.

## Como Fazer:
```PHP
<?php
$textoOriginal = "Olá, mundo! Php é incrível. Feliz programação PHP!";
$textoSubstituido = str_replace("PHP", "PHP 8", $textoOriginal);
echo $textoSubstituido; // Saída: Olá, mundo! Php 8 é incrível. Feliz programação PHP 8!
?>
```

Note que `str_replace` é sensível a maiúsculas e minúsculas. Para uma busca insensível à caixa, use `str_ireplace`:
```PHP
<?php
echo str_ireplace("php", "PHP 8", $textoOriginal); // Saída: Olá, mundo! PHP 8 é incrível. Feliz programação PHP 8!
?>
```

Para substituições mais complexas, pode ser necessário usar expressões regulares:
```PHP
<?php
$textoComNumeros = "Os números 2 e 4 serão substituídos.";
$textoAtualizado = preg_replace("/\d/", "substituído", $textoComNumeros);
echo $textoAtualizado; // Saída: Os números substituído e substituído serão substituídos.
?>
```

## Mergulho Profundo:
Buscar e substituir texto é uma funcionalidade fundamental que existe desde os primeiros dias da programação. Em PHP, as funções `str_replace` e `str_ireplace` são as ferramentas básicas para essas operações e são perfeitas para substituições simples e diretas. 

Por outro lado, as expressões regulares, manipuladas pela função `preg_replace`, oferecem uma potência incrível para padrões complexos e condições específicas, embora com um custo de desempenho adicional. Este mecanismo usa a biblioteca PCRE (Perl Compatible Regular Expressions), que traz a flexibilidade das expressões regulares do Perl para o PHP.

Alternativas incluem o uso de funções como `substr_replace` para substituir parte de uma string e a construção de funções customizadas, dependendo das necessidades específicas do projeto.

No tocante aos detalhes de implementação, é importante se atentar para a manipulação de strings Unicode, principalmente em aplicações multilíngues. O PHP 8 introduziu novas funcionalidades e melhorias no tratamento de strings, tornando-o mais amigável para a manipulação de dados em diferentes idiomas.

## Veja Também:
- [Documentação oficial do PHP para str_replace](https://www.php.net/manual/pt_BR/function.str-replace.php)
- [Documentação oficial do PHP para preg_replace](https://www.php.net/manual/pt_BR/function.preg-replace.php)
- [Tutorial sobre Expressões Regulares em PHP](https://www.phptutorial.net/php-regex/php-regular-expressions/)
- [Guia de migração do PHP 7.x para o PHP 8.x](https://www.php.net/manual/pt_BR/migration80.php)
