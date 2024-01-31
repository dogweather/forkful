---
title:                "Escrevendo testes"
date:                  2024-01-19
simple_title:         "Escrevendo testes"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (O Quê & Por Quê?)
Escrever testes é a prática de criar scripts automático que verificam se seu código se comporta como esperado. Programadores fazem isso para pegar bugs cedo, melhorar a qualidade do código e facilitar manutenções futuras.

## How to: (Como fazer:)
```C#
using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace MeuAppDeTestes {
    [TestClass]
    public class CalculadoraTestes {
        [TestMethod]
        public void TestaSoma() {
            var calculadora = new Calculadora();
            Assert.AreEqual(5, calculadora.Soma(2, 3));
        }
    }

    public class Calculadora {
        public int Soma(int a, int b) {
            return a + b;
        }
    }
}

// Saída esperada ao executar o teste:
// TestaSoma passou
```

## Deep Dive (Mergulho Profundo)
Os testes automáticos na programação começaram na década de 1950, mas só ganharam destaque nos anos 90 com o desenvolvimento de metodologias ágeis. Existem alternativas ao MSTest, como NUnit e xUnit, cada uma com suas especificidades. A escolha de uma depende de fatores como a familiaridade do time e recursos necessários. Implementar testes envolve seguir padrões como Arrange-Act-Assert (AAA) para estruturar o código de teste de forma clara.

## See Also (Veja Também)
- [Documentação do MSTest](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest)
- [Guia xUnit](https://xunit.net/docs/getting-started/netfx/visual-studio)
- [Princípios do teste de software ágil](https://www.agilealliance.org/glossary/tdd/)
