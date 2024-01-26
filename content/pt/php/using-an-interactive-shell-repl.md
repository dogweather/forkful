---
title:                "Usando um shell interativo (REPL)"
date:                  2024-01-26T04:16:25.458328-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Um shell interativo, ou REPL (Read-Eval-Print Loop, Ciclo de Leitura-Avaliação-Impressão), permite escrever e executar código PHP em tempo real. É ideal para experimentação, depuração ou aprendizado, pois você pode testar trechos de código sem o trabalho de criar um script completo.

## Como:
Inicie o REPL PHP executando `php -a` no seu terminal. Aqui está uma amostra de como ele funciona:

```php
php > echo "Olá, Mundo!";
Olá, Mundo!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Você também pode definir funções:

```php
php > function soma($a, $b) { return $a + $b; }
php > echo soma(5, 10);
15
```

## Aprofundando
Os REPLs existem de alguma forma desde os primeiros dias do LISP nos anos 60. O shell interativo do PHP é menos avançado em comparação com os de linguagens como Python ou JavaScript. Ele não mantém o estado entre sessões e carece de recursos como auto-completar. Para um REPL PHP mais repleto de recursos, considere alternativas como `psysh` ou `boris`. Esses shells de terceiros oferecem melhores ferramentas de introspecção, auto-completar e até mesmo um depurador.

Por trás das cortinas, o REPL do PHP funciona compilando e executando cada linha de código conforme ela é inserida. As limitações dessa abordagem se tornam claras com coisas como redeclarar classes, o que não é possível na mesma sessão. É ótimo para testes simples, mas pode se tornar oneroso para tarefas complexas.

## Veja Também
- [Manual do PHP - Shell interativo](https://www.php.net/manual/pt_BR/features.commandline.interactive.php)
- [PsySH: Um console de desenvolvimento em tempo de execução, depurador interativo e REPL para PHP](https://psysh.org/)
- [Boris: Um pequeno REPL para PHP](https://github.com/borisrepl/boris)