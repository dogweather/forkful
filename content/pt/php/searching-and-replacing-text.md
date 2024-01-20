---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Substituição de texto em PHP: Uma breve impressão

## O Quê & Por quê?
A pesquisa e substituição de texto é o processo de localizar strings específicas em um texto e substituí-las por outra string. Os programadores usam isso para modificar o conteúdo do texto, seja do código-fonte, conteúdo de páginas da web ou qualquer tipo de dados baseados em texto.

## Como fazer:
Aqui está um exemplo simples usando as funções `str_replace()` e `preg_replace()` do PHP.

```PHP
<?php
// Exemplo utilizando str_replace()
$texto = "O céu está azul";
$novoTexto = str_replace("azul", "vermelho", $texto);
echo $novoTexto; // Saída: O céu está vermelho

// Exemplo utilizando preg_replace()
$textoRegex = "A maçã pesa 5kg";
$novoTextoRegex = preg_replace("/[0-9]+/", "10", $textoRegex);
echo $novoTextoRegex; // Saída: A maçã pesa 10kg
?>
```

## Aprofundando
As funções `str_replace()` e `preg_replace()` foram introduzidas no PHP 4. Entretanto, enquanto `str_replace()` lida apenas com substituições simples de strings, `preg_replace()` também suporta expressões regulares.

Outras funções que podem ser utilizadas para busca e substituição de texto incluem `substr_replace()` e `strtr()`. No entanto, essas funções variam em termos de eficiência e flexibilidade.

Em termos de implementação, as funções de substituição do PHP são construídas sobre bibliotecas de strings de C, que são extremamente otimizadas para operações de strings.

## Veja também
Para mais informações sobre a substituição de texto em PHP, consulte os seguintes recursos:

- Documentação oficial do PHP: [php.net/manual/pt_BR/function.str-replace.php](https://www.php.net/manual/pt_BR/function.str-replace.php), [php.net/manual/pt_BR/function.preg-replace.php](https://www.php.net/manual/pt_BR/function.preg-replace.php)
- [stackoverflow.com](https://stackoverflow.com/questions/1252693/using-str-replace-so-that-it-only-acts-on-the-first-match) (em inglês): Exemplos de substituição de texto utilizando `str_replace()`