---
date: 2024-01-26 00:55:35.416869-07:00
description: "O tratamento de erros em PHP \xE9 sobre gerenciar e responder a condi\xE7\
  \xF5es que interrompem o fluxo normal de um programa, como arquivos ausentes ou\
  \ entrada de\u2026"
lastmod: '2024-03-11T00:14:20.389004-06:00'
model: gpt-4-1106-preview
summary: "O tratamento de erros em PHP \xE9 sobre gerenciar e responder a condi\xE7\
  \xF5es que interrompem o fluxo normal de um programa, como arquivos ausentes ou\
  \ entrada de\u2026"
title: Tratamento de erros
---

{{< edit_this_page >}}

## O Que & Por Quê?
O tratamento de erros em PHP é sobre gerenciar e responder a condições que interrompem o fluxo normal de um programa, como arquivos ausentes ou entrada de dados incorreta. Os programadores lidam com erros para evitar falhas e proporcionar uma experiência mais suave para os usuários.

## Como fazer:
No PHP, você pode gerenciar erros usando blocos `try-catch` e pode personalizar o processo com manipuladores de erro e exceções personalizados.

```php
// Exemplo básico de try-catch
try {
  // Fazer algo arriscado
  $file = fopen("arquivoinexistente.txt", "r");
} catch (Exception $e) {
  // Tratar o erro
  echo "Erro: " . $e->getMessage();
}

// Definindo um manipulador de erros personalizado
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// Usando exceções
class MinhaExcecao extends Exception {}

try {
  // Fazer algo e lançar uma exceção personalizada
  throw new MinhaExcecao("Erro personalizado!");
} catch (MinhaExcecao $e) {
  // Tratar a exceção personalizada
  echo $e->getMessage();
}

// Saída de exemplo:
// Erro: fopen(arquivoinexistente.txt): falha ao abrir stream: Arquivo ou diretório não encontrado
// Erro personalizado!
```

## Aprofundamento
Antigamente, os erros de PHP eram mais sobre avisos e notificações que não interrompiam a execução do script. Com a evolução da linguagem, ela adotou um tratamento de erro orientado a objetos mais robusto através da classe Exception introduzida no PHP 5. Posteriormente, o PHP 7 surgiu com classes de Erro que finalmente diferenciaram entre erros e exceções.

Antes dos blocos `try-catch`, o PHP usava `set_error_handler()` para lidar com erros. `try-catch` é mais limpo, mais moderno. Mas os manipuladores de erro personalizados ainda têm seu lugar, especialmente em código legado ou quando você precisa capturar o que normalmente seriam erros não excepcionais.

A interface `Throwable` no PHP 7+ significa que, seja um Erro ou Exceção, você pode capturar ambos. Isto é útil porque agora você não perde erros críticos de tempo de execução, que eram mais difíceis de rastrear antes.

Alternativas fora dos mecanismos incorporados do PHP incluem bibliotecas e frameworks que vêm com seus próprios sistemas de tratamento de erro, oferecendo mais recursos como registro de erros em arquivos ou exibindo páginas de erro amigáveis ao usuário.

## Veja Também
- Documentação oficial do PHP sobre Exceções: https://www.php.net/manual/pt_BR/language.exceptions.php
- PHP Do Jeito Certo sobre relatório de erros: https://phptherightway.com/#error_reporting
- Manual do PHP sobre Tratamento de Erros: https://www.php.net/manual/pt_BR/book.errorfunc.php
