---
title:    "PHP: Escrevendo testes."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em PHP?

Os testes são uma parte crucial da programação em PHP. Eles ajudam a garantir que o código esteja funcionando corretamente e a detectar erros antes que eles cheguem aos usuários. Além disso, eles também facilitam a manutenção e refatoração do código no futuro.

## Como escrever testes em PHP

Para escrever testes em PHP, você pode usar a estrutura de teste integrada do PHP, conhecida como PHPUnit. Primeiro, crie um arquivo de teste separado para cada função ou método que deseja testar. Em seguida, use a assertiva `assertEquals()` para comparar o resultado esperado com o resultado real. Aqui está um exemplo:

```PHP
function adicionar($a, $b) {
    return $a + $b;
}

class AdicionarTeste extends \PHPUnit\Framework\TestCase {
    public function testeAdicionar() {
        $resultado = adicionar(2, 3);
        $this->assertEquals(5, $resultado);
    }
}
```

O resultado do teste será positivo se o valor esperado e o valor real forem iguais, caso contrário, o teste falhará.

## Uma visão mais profunda sobre a escrita de testes

Existem várias técnicas e práticas para escrever testes em PHP de forma eficaz. A seguir, estão algumas dicas que podem ajudá-lo:

- Escreva testes para cada função ou método do seu código.
- Seja específico e cobrir todos os possíveis cenários de entrada para uma determinada função.
- Use a declaração `assert` adequada para cada tipo de teste (igualdade, identidade, exceção, etc.).
- Utilize o conceito de "teste de unidade" para testar pequenas partes isoladas do código.
- Considere o uso de mocks e stubs para simular cenários específicos durante os testes.

Com essas práticas em mente, você poderá criar testes confiáveis e abrangentes para o seu código PHP.

## Veja também

- [Documentação do PHPUnit](https://phpunit.de/manual/7.5/en/index.html)
- [Laracasts - PHP Testing Crash course](https://laracasts.com/series/phpunit-testing-in-php)