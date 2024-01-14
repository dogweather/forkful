---
title:                "PHP: Procurando e substituindo texto"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que fazer busca e substituição de texto?

Fazer busca e substituição de texto em programas PHP é uma importante habilidade para qualquer desenvolvedor. Com esta técnica, é possível encontrar e substituir blocos de texto específicos em um conjunto de dados, poupando tempo e esforço em processos de edição manual.

## Como fazer busca e substituição em PHP

Fazer busca e substituição em PHP é um processo simples e eficiente. Para realizar essa tarefa, é preciso usar duas funções nativas do PHP: ```str_replace()``` e ```preg_replace()```. Vamos ver como cada uma delas funciona.

```
<?php
// Usando a função str_replace()
$texto = "O rato roeu a roupa do rei de Roma";
$resultado = str_replace("rato", "gato", $texto);
echo $resultado;

// Saída: O gato roeu a roupa do rei de Roma

// Usando a função preg_replace()
$texto = "Ligue para (11) 99999-9999 para falar com nosso atendimento";
$resultado = preg_replace("/(\([0-9]{2}\)) ([0-9]{4,5}-[0-9]{4})/", "Sua ligação foi encaminhada para o número $2", $texto);
echo $resultado;

// Saída: Ligue para Sua ligação foi encaminhada para o número 99999-9999 para falar com nosso atendimento
?>
```

Como podemos ver nos exemplos acima, a função ```str_replace()``` substitui todas as ocorrências de uma palavra por outra, enquanto a função ```preg_replace()``` usa expressões regulares para encontrar e substituir padrões. Você também pode usar essas funções em conjunto com outras funções de manipulação de strings do PHP, como ```explode()``` e ```implode()```, para ter ainda mais controle sobre o processo de busca e substituição.

## Aprofundando na busca e substituição em PHP

Para aqueles que desejam aprofundar seus conhecimentos em busca e substituição em PHP, é importante aprender a usar expressões regulares. Com elas, é possível encontrar e substituir padrões específicos em um texto, tornando o processo ainda mais versátil e poderoso. Além disso, é importante estar sempre atualizado sobre as diferentes funções nativas do PHP, pois novas versões sempre trazem melhorias e novidades úteis para o desenvolvimento.

## Veja também

- [Documentação oficial do PHP: Funções de manipulação de strings](https://www.php.net/manual/pt_BR/ref.strings.php)
- [Tutorial: Busca e substituição em PHP](https://www.php.net/manual/pt_BR/function.preg-replace)
- [Site com expressões regulares interativas](https://regexr.com/)