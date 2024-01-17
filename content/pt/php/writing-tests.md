---
title:                "Escrevendo testes"
html_title:           "PHP: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-tests.md"
---

{{< edit_this_page >}}

#O que & Por quê?

Escrever testes é uma prática comum entre os programadores de PHP. Esse processo consiste em criar um conjunto de instruções que verificam se o código do programa está funcionando corretamente. Os testes são importantes porque garantem que o código está livre de erros e podem ser usados para detectar falhas antes que elas se tornem um problema maior.

#Como fazer:

Segue abaixo um exemplo de código PHP para criar um teste simples e imprimir o resultado:

```PHP
<?php
function calcularSucesso($nota) {
	if ($nota >= 70) {
		return "Aprovado";
	} else {
		return "Reprovado";
	}
}

echo calcularSucesso(85);

// Output:
// Aprovado
?>
```

Neste exemplo, a função `calcularSucesso` verifica se a nota passada como parâmetro é maior ou igual a 70. Se for, ela retorna a string "Aprovado", caso contrário retorna "Reprovado". Depois, a função é chamada e o resultado é impresso na tela.

#Mergulho Profundo:

Os testes surgiram na indústria de software por volta dos anos 60, mas só se tornaram populares nos anos 90 com o advento das metodologias ágeis. Existem diversas ferramentas de teste disponíveis no mercado, como o PHPUnit e o Codeception. Além disso, existem diferentes tipos de testes, como testes unitários, testes de integração e testes de aceitação.

É importante destacar que escrever testes não garante que o código está livre de erros, mas ajuda a identificá-los mais facilmente para que possam ser corrigidos antes que causem problemas mais graves. Alguns programadores preferem não escrever testes, mas essa prática pode levar a erros que dificultam a manutenção e fazem com que o código de um projeto se torne cada vez mais complexo.

#Veja Também:

- Documentação oficial do PHPUnit: https://phpunit.de/documentation.html
- Introdução aos testes em PHP com PHPUnit: https://code.tutsplus.com/pt/tutorials/introduction-to-web-testing-with-phpunit--net-6367
- Tutorial sobre testes de unidade com PHP e PHPUnit: https://www.toptal.com/php/integration-and-unit-testing-guide-for-php