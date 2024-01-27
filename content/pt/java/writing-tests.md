---
title:                "Escrevendo testes"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-tests.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Escrever testes é criar código para verificar se outro código funciona como esperado. Programadores fazem isso para evitar bugs, simplificar atualizações e entregar com confiança.

## Como Fazer:

Vamos usar JUnit 5, o framework de testes mais recente e popular para Java. Considere uma classe simples `Calculadora` que queremos testar:

```Java
public class Calculadora {
    public int somar(int a, int b) {
        return a + b;
    }
}
```

Agora, o teste unitário para o método `somar` pode ser assim:

```Java
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

public class CalculadoraTest {

    @Test
    void testSomar() {
        Calculadora calc = new Calculadora();
        assertEquals(5, calc.somar(2, 3), "2 + 3 deve ser igual a 5");
    }
}
```

Se o teste passar, a saída será simplesmente uma confirmação de que todos os testes foram concluídos com sucesso. Aqui está a saída esperada se tudo estiver correto:

```
Test run finished after 40 ms
[         3 containers found      ]
[         0 containers skipped    ]
[         3 containers started    ]
[         0 containers aborted    ]
[         3 containers successful ]
[         0 containers failed     ]
[         5 tests found           ]
[         0 tests skipped         ]
[         5 tests started         ]
[         0 tests aborted         ]
[         5 tests successful      ]
[         0 tests failed          ]
```

## Mergulho Profundo:

Os testes são importantes desde os dias da programação em cartões perfurados, onde cada erro custava tempo e dinheiro. Hoje, temos várias abordagens e frameworks, sendo JUnit o mais popular em Java. Embora JUnit seja amplamente usado para testes unitários, outras ferramentas como TestNG ou frameworks de BDD (Behavior-Driven Development) como Cucumber oferecem alternativas poderosas. Ao escrever testes, detalhes importantes incluem isolamento (cada teste deve ser independente), determinismo (resultados consistentes) e cobertura de código completa.

## Veja Também:

- Documentação oficial do JUnit 5: https://junit.org/junit5/docs/current/user-guide/
- Tutorial de TDD (Test-Driven Development) com Java: https://www.baeldung.com/java-tdd
- Comparação entre frameworks de testes: https://dzone.com/articles/top-8-java-testing-frameworks-for-developers
