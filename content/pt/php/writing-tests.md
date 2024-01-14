---
title:                "PHP: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-tests.md"
---

{{< edit_this_page >}}

# Por que escrever testes em PHP?

Se você é um programador PHP, com certeza já ouviu falar sobre a importância de escrever testes. Mas você já se perguntou, afinal, por que é tão importante dedicar tempo e esforço para escrevê-los? Neste post, vamos explorar as razões pelas quais escrever testes é fundamental para o desenvolvimento de software.

## Como escrever testes em PHP

Escrever testes em PHP pode parecer intimidante à primeira vista, mas na verdade é bastante simples. Primeiro, é necessário escolher uma ferramenta de testes, como PHPUnit ou Codeception, que são amplamente utilizadas pela comunidade PHP. Em seguida, é preciso criar os arquivos de testes dentro do diretório de testes da sua aplicação.

Para escrever um teste, é necessário definir uma classe com o prefixo "Test" e um método com o prefixo "test" seguido pelo nome da funcionalidade que será testada. Dentro desse método, são utilizadas as funções de assertivas para verificar se o resultado esperado é igual ao obtido.

Abaixo, um exemplo de teste para a função de soma em PHP utilizando o PHPUnit:

```PHP
class SomaTest extends PHPUnit_Framework_TestCase
{
    public function testSoma()
    {
        $resultado = soma(2,3);
        $this->assertEquals(5, $resultado);
    }
}
```

Após escrever os testes, é possível rodá-los através de um comando no terminal ou utilizando uma ferramenta de integração contínua. Isso garantirá que suas alterações no código não impactem no funcionamento de funcionalidades já existentes.

## Aprofundando nos testes

Além de garantir o bom funcionamento do código, escrever testes também traz outros benefícios, como:

- Facilitar a detecção e correção de bugs;
- Servir como documentação do código;
- Possibilitar a refatoração do código com mais segurança.

Outra vantagem importante dos testes é que eles permitem a prática de desenvolvimento orientado a testes (TDD), onde os testes são escritos antes mesmo do código ser implementado. Isso garante que o código seja mais bem pensado e estruturado, resultando em um software de maior qualidade.

Por fim, é importante mencionar que escrever testes não é algo a ser feito somente em projetos profissionais. Mesmo em projetos pessoais ou de estudo, é uma prática que deve ser adotada para desenvolver habilidades e garantir a qualidade do seu código.

# Veja também

- [PHPUnit - Documentação oficial](https://phpunit.de/documentation.html)
- [Codeception - Documentação oficial](https://codeception.com/docs/)
- [Desenvolvimento orientado a testes (TDD)](https://www.devmedia.com.br/desenvolvimento-orientado-a-testes-tdd-artigo/18597)
- [Integração contínua e testes](https://imasters.com.br/desenvolvimento/integracao-continua-e-testes-o-guia-completo/)