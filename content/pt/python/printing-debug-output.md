---
title:                "Python: Imprimindo saída de depuração"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração é importante?

A impressão de saída de depuração é uma ferramenta essencial para a identificação e correção de erros em seus códigos Python. Ao imprimir trechos específicos de seus códigos, você pode ter uma visão mais clara do que está acontecendo em cada etapa da execução do programa. Além disso, é uma forma eficaz de verificar se os valores das variáveis estão corretos e se o código está seguindo o fluxo esperado.

## Como fazer isso?

Imprimir saída de depuração é uma tarefa simples em Python. Basta usar a função `print()` e adicionar os valores ou variáveis que deseja visualizar. Aqui está um exemplo de código:

```python
# Declaração de variáveis
nome = "Maria"
idade = 25
altura = 1.60

# Imprimindo saída de depuração
print("O nome é:", nome)
print("A idade é:", idade)
print("A altura é:", altura)
```

E aqui está o resultado da saída de depuração:

```
O nome é: Maria
A idade é: 25
A altura é: 1.60
```

Como você pode ver, a função `print()` exibe as informações que adicionamos entre parênteses na saída. Isso pode ser muito útil para verificar se os valores das variáveis são os esperados ou se algum erro ocorreu durante a execução do código.

## Mergulhando mais fundo

Existem algumas dicas e boas práticas que podem ajudar a tornar a impressão de saída de depuração mais eficaz:

- Adicione expressões ou mensagens claras na saída para torná-la mais fácil de entender.
- Use a formatação de string para ter um controle maior sobre a aparência da saída.
- Organize a saída em várias linhas para torná-la mais legível.
- Ao terminar a depuração, lembre-se de remover todas as instruções de impressão de saída para evitar que poluam seu código.

## Ver também

- Documentação oficial de impressão em Python: https://docs.python.org/3/library/functions.html#print
- Tutoriais de debug em Python: https://code.visualstudio.com/docs/python/python-tutorial