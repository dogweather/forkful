---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:33.193945-07:00
description: "Express\xF5es regulares (regex) em PHP s\xE3o padr\xF5es usados para\
  \ combinar combina\xE7\xF5es de caracteres em strings, permitindo opera\xE7\xF5\
  es sofisticadas de busca e\u2026"
lastmod: '2024-02-25T18:49:44.277939-07:00'
model: gpt-4-0125-preview
summary: "Express\xF5es regulares (regex) em PHP s\xE3o padr\xF5es usados para combinar\
  \ combina\xE7\xF5es de caracteres em strings, permitindo opera\xE7\xF5es sofisticadas\
  \ de busca e\u2026"
title: "Usando express\xF5es regulares"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Expressões regulares (regex) em PHP são padrões usados para combinar combinações de caracteres em strings, permitindo operações sofisticadas de busca e substituição e validação de dados. Programadores utilizam regex pela sua potência e flexibilidade na análise de texto, validação de formulários ou raspagem de dados da web, tornando-o uma ferramenta indispensável no arsenal de um desenvolvedor.

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
