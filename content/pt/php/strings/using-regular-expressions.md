---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:33.193945-07:00
description: "Como Fazer: O PHP suporta express\xF5es regulares atrav\xE9s da biblioteca\
  \ PCRE (Perl Compatible Regular Expressions), oferecendo um rico conjunto de fun\xE7\
  \xF5es.\u2026"
lastmod: '2024-03-13T22:44:46.657531-06:00'
model: gpt-4-0125-preview
summary: "O PHP suporta express\xF5es regulares atrav\xE9s da biblioteca PCRE (Perl\
  \ Compatible Regular Expressions), oferecendo um rico conjunto de fun\xE7\xF5es."
title: "Usando express\xF5es regulares"
weight: 11
---

## Como Fazer:
O PHP suporta expressões regulares através da biblioteca PCRE (Perl Compatible Regular Expressions), oferecendo um rico conjunto de funções. Veja como usá-las:

### Correspondência de um padrão:
Para verificar se um padrão existe dentro de uma string, use `preg_match()`. Esta função retorna 1 se o padrão foi encontrado na string e 0 se não.

```php
if (preg_match("/\bweb\b/i", "PHP é uma linguagem de script para web")) {
    echo "Uma correspondência foi encontrada.";
} else {
    echo "Uma correspondência não foi encontrada.";
}
// Saída: Uma correspondência foi encontrada.
```

### Encontrando todas as correspondências:
`preg_match_all()` é usado quando você precisa encontrar todas as ocorrências de um padrão dentro de uma string.

```php
$text = "gatos e cachorros";
$padrão = "/\b([a-z]+)\b/i";
preg_match_all($padrão, $text, $matches);
print_r($matches[0]);
// Saída: Array ( [0] => gatos [1] => e [2] => cachorros )
```

### Substituindo texto:
Para substituir o texto que corresponde a uma expressão regular, `preg_replace()` é usado. É incrivelmente poderoso para formatar e limpar dados.

```php
$textoOriginal = "15 de abril de 2003";
$padrão = "/(\w+) (\d+), (\d+)/i";
$substituição = '${1}1,$3';
echo preg_replace($padrão, substituição, $textoOriginal);
// Saída: 15 de abril1,2003
```

### Dividindo strings:
Você pode dividir uma string em um array usando `preg_split()`, especificando um padrão para o delimitador.

```php
$text = "PHP é, extremamente popular, linguagem de script";
$partes = preg_split("/,\s*/", $text);
print_r($partes);
// Saída: Array ( [0] => PHP é [1] => extremamente popular [2] => linguagem de script )
```

Além disso, para padrões de regex complexos e tarefas, frameworks e bibliotecas como o componente `Finder` do Symfony ou a coleção de funções auxiliares do Laravel podem oferecer uma camada de abstração mais conveniente. No entanto, compreender e utilizar as funções PCRE integradas do PHP é crucial para o processamento de texto eficiente e validação diretamente nos scripts PHP.
