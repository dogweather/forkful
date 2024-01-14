---
title:    "PHP: Unindo strings"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/concatenating-strings.md"
---

{{< edit_this_page >}}

##Por que

Muitas vezes, em programação, é necessário combinar várias strings em uma única string. Isso pode ser feito por uma variedade de razões, como formatar um texto, criar uma URL ou construir uma consulta de banco de dados. Em PHP, a concatenação de strings é uma técnica importante e útil que você deve conhecer.

##Como Fazer

Em PHP, a concatenação de strings é realizada usando o operador "." (ponto). Isso permite que você combine duas ou mais strings em uma única string. Veja o exemplo abaixo:

```PHP
$nome = "João";
$sobrenome = "Silva";
//concatenando as strings
$nome_completo = $nome . " " . $sobrenome;
echo $nome_completo;
//saída: João Silva
```

Você também pode usar a função `concat()` para realizar a concatenação:

```PHP
$nome = "Maria";
$sobrenome = "Santos";
//concatenando as strings
$nome_completo = concat($nome, " ", $sobrenome);
echo $nome_completo;
//saída: Maria Santos
```

Além disso, é possível usar o operador de atribuição composto `.=` para acrescentar uma string a outra já existente:

```PHP
$frase = "Eu amo ";
$lugar = "Portugal";
//concatenando as strings
$frase .= $lugar;
echo $frase;
//saída: Eu amo Portugal
```

Lembrando que a concatenação também pode ser feita com variáveis que contêm números ou outros tipos de dados, não apenas com strings.

##Mergulho Profundo

No PHP, a concatenação de strings é um processo relativamente eficiente, mas é importante se atentar a alguns detalhes para evitar erros:

- Verifique se as variáveis que serão concatenadas existem e estão definidas corretamente antes de realizar a concatenação.
- Use aspas simples (`'`) em vez de aspas duplas (`"`) quando possível, pois isso é mais rápido e só avaliará a string literal, em vez de qualquer variável dentro da string.
- Use o operador de concatenação somente se não for possível combinar as strings usando a função `printf()` ou as concatenando com a vírgula (`,`).

Seguindo essas dicas, você estará apto a concatenar strings eficientemente e sem problemas.

##Veja Também

- [Documentação do PHP para Concatenação de Strings](https://www.php.net/manual/pt_BR/language.operators.string.php)
- [Vídeo Tutorial: Concatenação de Strings em PHP](https://www.youtube.com/watch?v=8kqS5YWnA1A)
- [Guia de Referência para PHP: Função Concat()](https://www.php.net/manual/pt_BR/function.concat.php)