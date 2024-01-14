---
title:                "PHP: Escrevendo testes"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante?

Escrever testes é uma parte essencial de programação e pode trazer muitos benefícios para os desenvolvedores. Testes bem escritos podem fornecer uma maior confiabilidade do código, facilitando a identificação e correção de erros, além de tornar o processo de desenvolvimento mais eficiente e produtivo.

## Como escrever testes em PHP

Incluir testes em nossos códigos PHP é uma tarefa relativamente simples. Podemos começar definindo uma função de teste usando o comando `assert()` e passando como parâmetro a expressão que desejamos testar. Vejamos um exemplo abaixo:

```PHP
$expected = 10;
$result = soma(5, 5);

assert($result == $expected);
```

Neste exemplo, estamos testando a função `soma()` para garantir que ela retorne o valor esperado. Se a expressão passada no comando `assert()` for avaliada como falsa, o teste falhará e receberemos uma mensagem de erro indicando a linha e o arquivo onde o erro ocorreu.

## Aprofundando na escrita de testes

Além do `assert()`, o PHP também possui outros métodos de testes disponíveis, como `assertContains()` e `assertNotEquals()`. Também é possível criar nossos próprios métodos de testes personalizados, melhorando ainda mais a eficácia dos nossos testes.

Outro conceito importante na escrita de testes é o de "cobertura de código". Isso se refere à porcentagem do nosso código que é testada pelos testes. É importante ter uma alta cobertura de código para garantir que todas as partes do nosso código estejam funcionando corretamente.

Por fim, é importante lembrar de manter os testes atualizados conforme o código for sendo alterado, garantindo assim que todos os testes continuem funcionando e oferecendo a confiabilidade necessária ao nosso código.

## Veja também

- [Documentação oficial do PHP sobre testes](https://www.php.net/manual/pt_BR/book.simplexml.php)
- [Artigo sobre boas práticas para escrever testes em PHP](https://blog.locaweb.com.br/desenvolvimento/boas-praticas-para-escrever-testes-em-php/)
- [Vídeo explicando conceitos avançados de testes em PHP](https://www.youtube.com/watch?v=aTR5TmACWWI)