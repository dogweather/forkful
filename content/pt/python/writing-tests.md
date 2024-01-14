---
title:                "Python: Escrevendo testes"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Python?

Se você é um programador Python, provavelmente já ouviu falar sobre a importância de escrever testes para o seu código. Mas por que isso é realmente necessário? Bem, existem vários motivos pelos quais escrever testes é uma boa prática em qualquer linguagem de programação, mas aqui estão os principais motivos pelos quais você deve considerar escrever testes em Python:

- Testes ajudam a identificar e corrigir bugs: Ao escrever testes, você pode simular diferentes cenários e garantir que seu código funcione corretamente em todas as situações possíveis. Isso ajuda a evitar bugs e, se houver algum, facilita sua identificação e correção.
- Testes fornecem documentação do código: Ao ler seus testes, outras pessoas podem entender melhor o que seu código faz e como ele deve ser usado. Isso é especialmente útil quando você trabalha em equipe.
- Testes permitem alterar o código sem medo: Com testes adequados, você pode fazer alterações no seu código sem medo de introduzir novos bugs, pois seus testes garantem que o comportamento esperado do código não seja alterado.

## Como escrever testes em Python

Agora que você sabe por que escrever testes é importante, vamos dar uma olhada em como escrevê-los em Python. Suponha que temos uma função simples que retorna o dobro de um número dado:

```Python
def double(num):
    return num * 2
```

Para testar essa função, podemos criar um arquivo de teste separado, onde importamos a função e escrevemos diferentes casos de teste usando a biblioteca padrão de testes do Python, chamada `unittest`:

```Python
import unittest
from calculator import double # importa a função que queremos testar

class TestCalculator(unittest.TestCase):

    def test_positive_number(self):
        self.assertEqual(double(2), 4) # verifica se o resultado é 4

    def test_negative_number(self):
        self.assertEqual(double(-3), -6) # verifica se o resultado é -6

    def test_zero(self):
        self.assertEqual(double(0), 0) # verifica se o resultado é 0

if __name__ == '__main__':
    unittest.main()
```

Ao executar esse arquivo de teste, se tudo estiver correto, você verá a seguinte saída:

```
...
----------------------------------------------------------------------
Ran 3 tests in 0.000s

OK
```

Caso algo esteja errado, o pytest mostrará qual teste falhou e por quê, o que facilita a correção do problema.

## Deep Dive: Dicas para escrever testes eficazes em Python

Aqui estão algumas dicas adicionais que podem ajudá-lo a escrever testes eficazes em Python:

- Escreva testes para casos extremos e de borda, não apenas para casos típicos.
- Teste pequenas partes do código de cada vez, em vez de testar tudo de uma vez.
- Use nomes descritivos para seus testes e casos de teste, isso tornará seu código mais legível.
- Seja consistente com a estrutura e organização dos seus testes.
- Ao mudar o código, certifique-se de que seus testes ainda passem.

## Veja também

Aqui estão alguns links úteis para continuar aprendendo sobre testes em Python:

- [Documentação oficial do unittest](https://docs.python.org/3/library/unittest.html)
- [Tutorial sobre testes em Python](https://realpython.com/python-testing/)
- [Vídeo sobre testes de unidade em Python](https://www.youtube.com/watch?v=1Lfv5tUGsn8)