---
date: 2024-01-26 03:50:31.380799-07:00
description: "Um depurador \xE9 uma ferramenta que ajuda os programadores a entender\
  \ o que seu c\xF3digo est\xE1 realmente fazendo enquanto \xE9 executado. \xC9 a\
  \ lupa que nos permite\u2026"
lastmod: '2024-03-11T00:14:20.386043-06:00'
model: gpt-4-0125-preview
summary: "Um depurador \xE9 uma ferramenta que ajuda os programadores a entender o\
  \ que seu c\xF3digo est\xE1 realmente fazendo enquanto \xE9 executado. \xC9 a lupa\
  \ que nos permite\u2026"
title: Usando um depurador
---

{{< edit_this_page >}}

## O Que & Por Que?
Um depurador é uma ferramenta que ajuda os programadores a entender o que seu código está realmente fazendo enquanto é executado. É a lupa que nos permite focar em bugs—esses problemas irritantes que fazem nossos programas travarem ou cuspirem respostas erradas—e eliminá-los. Usamos depuradores porque eles nos poupam horas de declarações de impressão e jogos de adivinhação.

## Como fazer:
O PHP vem com um depurador interativo chamado Xdebug. Aqui está como usá-lo.

Primeiro, verifique se você tem o Xdebug instalado e configurado no seu arquivo `php.ini`:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

Em seguida, escreva um script PHP simples com um bug:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // Ops! Isso deveria ser uma soma, não uma subtração
}

$result = add(1, 2);
echo "O resultado é: $result"; // A saída deveria ser 3, não -1
```

Usando uma IDE como o PhpStorm, defina um ponto de interrupção clicando ao lado do número da linha. Execute o depurador e observe como as variáveis mudam à medida que você avança na execução. Quando você passa pela função `add`, perceberá que `$result` se torna -1, o que é inesperado.

## Aprofundamento:
Historicamente, o PHP era usado principalmente para scripts pequenos, e a depuração era uma questão de adicionar declarações `var_dump()` e `print_r()` pelo código. Com o tempo, com o PHP se tornando um jogador chave no desenvolvimento web, ferramentas mais sofisticadas como Xdebug e Zend Debugger entraram em uso.

Alternativas ao Xdebug incluem pcov e phpdbg. Estes oferecem várias funcionalidades, mas podem não ser tão completos quanto o Xdebug. phpdbg é um depurador específico para PHP, leve, distribuído com o PHP desde a versão 5.6, e pcov é um driver de cobertura de código.

Ao implementar um depurador, lembre-se de que você nunca deve deixar o depurador ativado no seu servidor de produção, pois isso pode expor vulnerabilidades de segurança e diminuir o desempenho.

## Veja Também:
- [Documentação do Xdebug](https://xdebug.org/docs/)
- [Guia de Depuração do PhpStorm](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net sobre phpdbg](https://www.php.net/manual/pt_BR/book.phpdbg.php)
- [pcov no GitHub](https://github.com/krakjoe/pcov)
