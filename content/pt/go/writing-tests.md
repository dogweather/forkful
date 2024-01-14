---
title:                "Go: Escrevendo testes"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante

O ato de escrever testes é uma parte crucial do processo de desenvolvimento de software. Ele garante que seu código esteja funcionando corretamente e evita erros e falhas no futuro. Além disso, escrever testes pode ajudá-lo a entender melhor o funcionamento do seu código e melhorar sua habilidade como programador.

## Como escrever testes em Go

Escrever testes em Go é muito fácil e pode ser feito usando o pacote `testing`, que já vem incluído na biblioteca padrão da linguagem. Veja um exemplo de como escrever um teste simples para uma função:

```Go
func Soma(a, b int) int {
    return a + b
}

func TestSoma(t *testing.T) {
    resultado := Soma(2, 2)
    esperado := 4
    if resultado != esperado {
        t.Errorf("Resultado incorreto. Esperado: %d, obtido: %d", esperado, resultado)
    }
}
```

Na primeira função, `Soma`, criamos uma função simples que recebe dois números inteiros e retorna a soma deles. Na segunda função, `TestSoma`, usamos o pacote `testing` para verificar se a função `Soma` está funcionando corretamente. O teste é falhado se o resultado obtido for diferente do esperado.

Este é apenas um exemplo básico, mas é possível escrever testes mais complexos e abrangentes para verificar diversas funcionalidades do seu código.

## Aprofundando-se nos testes

Além de apenas verificar o resultado esperado, também é possível testar possíveis erros e falhas no seu código. Para isso, o pacote `testing` oferece a função `Errorf`, que permite exibir uma mensagem de erro personalizada se algo der errado no teste.

Também é importante lembrar de testar as diferentes condições possíveis, como entradas inválidas ou valores extremos. Isso garantirá que seu código seja robusto e lide com todas as possibilidades.

Além disso, é possível utilizar ferramentas adicionais, como o pacote `testing/quick`, para gerar entradas aleatórias e testar seu código em uma variedade maior de situações.

## Veja também

- [Documentação do pacote `testing` em Go](https://pkg.go.dev/testing)
- [Tutorial sobre testes em Go](https://gobyexample.com/testing)
- [Vídeo explicando a importância de escrever testes em programas](https://www.youtube.com/watch?v=_NnElPO5BU0)