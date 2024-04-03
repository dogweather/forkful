---
date: 2024-01-26 04:44:06.874603-07:00
description: "N\xFAmeros complexos t\xEAm uma parte real e uma parte imagin\xE1ria,\
  \ geralmente escritos como `a + bi`. Eles s\xE3o cruciais em matem\xE1tica avan\xE7\
  ada, f\xEDsica,\u2026"
lastmod: '2024-03-13T22:44:46.661371-06:00'
model: gpt-4-0125-preview
summary: "N\xFAmeros complexos t\xEAm uma parte real e uma parte imagin\xE1ria, geralmente\
  \ escritos como `a + bi`."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## O Que & Por Quê?
Números complexos têm uma parte real e uma parte imaginária, geralmente escritos como `a + bi`. Eles são cruciais em matemática avançada, física, engenharia e certos algoritmos de computador. Programadores trabalham com eles para lidar com cálculos que envolvem raízes quadradas de números negativos e funções oscilantes.

## Como:
O PHP oferece suporte integrado para números complexos usando a extensão `ext-intl` com a classe `NumberFormatter`. Aqui está um exemplo:

```php
// Certifique-se de que a extensão intl está carregada
if (!extension_loaded('intl')) {
    die("A extensão intl não está habilitada. Por favor, habilite-a para executar este código.");
}

function addComplexNumbers($a, $b) {
    // Use NumberFormatter para analisar e formatar números complexos
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // Analisa números complexos a partir de strings
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // Realiza a adição
    $sum = $numA + $numB;

    // Formata o resultado como um número complexo
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // Saída: 7+10i
```

## Aprofundamento
Antes de `ext-intl`, o PHP não tinha suporte nativo para números complexos. Desenvolvedores usavam funções ou bibliotecas de classes personalizadas para lidar com números complexos. Operações complexas poderiam ser tediosas e propensas a erros, mas `ext-intl` oferece uma maneira internacionalizada de apresentar e analisar números complexos alinhados com a biblioteca ICU.

No entanto, para operações matemáticas pesadas, alguns podem usar bibliotecas externas escritas em linguagens mais amigáveis à matemática (como C ou Python) e interagir com elas através do PHP. Quanto à implementação, `ext-intl` lida com isso nos bastidores, garantindo aritmética precisa enquanto abstrai a complexidade do desenvolvedor.

Historicamente, os números complexos eram malvistos, sendo denominados 'imaginários', mas desde então tornaram-se fundamentais em vários campos científicos e matemáticos, revelando mais sobre sua significância no mundo real do que o status de imaginário jamais sugeriu.

## Veja Também
- [Manual do PHP sobre NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipédia sobre números complexos](https://en.wikipedia.org/wiki/Complex_number)
- [PHP: Do Jeito Certo - Trabalhando com Tipos de Dados](https://phptherightway.com/#data_types)
