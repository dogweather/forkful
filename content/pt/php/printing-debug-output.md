---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/printing-debug-output.md"
---

{{< edit_this_page >}}

---

# Artigo de Programação PHP: Impressão de Depuração de Saída

---

## O Que & Porquê? 

A impressão de depuração de saída é uma técnica para rastrear os valores das variáveis e o fluxo de execução de um script. Os programadores usam-na para identificar e diagnosticar erros ou comportamento inesperado em seu código.

---

## Como fazer:

Vamos olhar para alguns exemplos de código para saber como imprimir a depuração de saída em PHP.

```PHP
<?php
$variavel = 'Olá, Mundo PHP!';
echo $variavel;
```
O output deste script será: `Olá, Mundo PHP!`

Quando precisar depurar um array ou objeto complexo, poderá usar `print_r()` ou `var_dump()`.

```PHP
<?php
$array = array('foo', 'bar', 'baz');
print_r($array);
```
O output deste script será: `Array ( [0] => foo [1] => bar [2] => baz )`

---

## Mergulho Profundo 

Historicamente, a impressão de depuração de saída tem sido uma ferramenta essencial para os programadores PHP. Antes dos IDEs modernos e das ferramentas de depuração mais sofisticadas, eram frequentemente as únicas maneiras de diagnosticar problemas.

Em relação a alternativas, existem diversas ferramentas de depuração como Xdebug, que fornece muitas funcionalidades além da impressão de depuração de saída. No entanto, às vezes, uma rápida chamada de `echo` ou `print_r()` é tudo que você precisa.

Falando sobre detalhes de implementação, `print_r` e `var_dump` são construções de linguagem incorporadas ao núcleo do PHP. O `var_dump` fornece mais informações do que `print_r`, exibindo o tipo e o tamanho de qualquer variável, além do seu valor.

---

## Veja Também

1. [Documentação PHP: Função Print](https://www.php.net/manual/pt_BR/function.print.php)
2. [Documentação PHP: Função Echo](https://www.php.net/manual/pt_BR/function.echo.php)
3. [Documentação PHP: Função print_r](https://www.php.net/manual/pt_BR/function.print-r.php)
4. [Documentação PHP: Função var_dump](https://www.php.net/manual/pt_BR/function.var-dump.php)
5. [Xdebug, uma ferramenta de depuração PHP](https://xdebug.org/) 

---

Os links para os tutoriais e a documentação podem ajudar a compreender melhor esses conceitos e a aplicá-los em seu código. Lembre-se, a depuração é uma habilidade central em programação. Quanto melhor você usar, melhor você programará.