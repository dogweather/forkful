---
title:                "Escrevendo testes"
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-tests.md"
---

{{< edit_this_page >}}

## O Que é e Por Quê?
Escrever testes é sobre criar scripts que automaticamente verificam se o código faz o que deve fazer. Programadores fazem isso para garantir a qualidade, fazer refatorações sem medo e economizar tempo ao evitar bugs futuros.

## Como Fazer:
```PHP
<?php
use PHPUnit\Framework\TestCase;

class ExemploTest extends TestCase
{
    public function testSoma()
    {
       $this->assertEquals(4, soma(2, 2));
    }
}

function soma($a, $b) {
    return $a + $b;
}
?>
```
Saída esperada: OK (1 test, 1 assertion)

## Aprofundamento
Os testes automáticos começaram nos anos 60 mas só ganharam força com o desenvolvimento ágil nos anos 2000. Alternativas ao PHPUnit incluem o PHPSpec e o Codeception, mas o PHPUnit é o padrão de fato com grande apoio comunitário e integração contínua. Ao escrever testes, detalhes como cobertura de código e mock objects são fundamentais para simular e verificar todas as possíveis operações do código.

## Veja Também
- [PHPUnit – The PHP Testing Framework](https://phpunit.de/)
- [Martin Fowler – Test Pyramid](https://martinfowler.com/articles/practical-test-pyramid.html)
- [Codeception – Full-stack testing PHP framework](https://codeception.com/)
