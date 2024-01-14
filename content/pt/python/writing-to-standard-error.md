---
title:    "Python: Escrevendo para o erro padrão"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão

Escrever para o erro padrão é uma habilidade importante em programação Python. Ao escrever mensagens de erro corretas, você pode garantir que os usuários do seu programa possam identificar e resolver problemas com mais facilidade, melhorando a experiência geral do usuário.

## Como fazer

Para escrever para o erro padrão em Python, você precisará usar a função `print` combinada com o parâmetro `file=sys.stderr`. Aqui está um exemplo simples:

```Python
print("Houve um erro!", file=sys.stderr)
```

Isso imprimirá a mensagem "Houve um erro!" no console, indicando que ocorreu um erro. Você pode personalizar sua mensagem de erro como quiser e até mesmo incluir informações adicionais para ajudar os usuários a identificar o problema.

```Python
print("O valor inserido é inválido. Insira um número positivo.", file=sys.stderr)
```

Isso imprimirá a mensagem "O valor inserido é inválido. Insira um número positivo." no console, indicando que um valor inválido foi inserido pelo usuário.

## Mergulho profundo

Ao escrever para o erro padrão, é importante ter em mente que as mensagens devem ser claras e concisas. Evite usar gírias ou terminologia técnica que possa confundir os usuários. É sempre útil incluir informações específicas sobre o erro, como a linha de código onde ocorreu, para que os usuários possam corrigi-lo mais facilmente.

Além disso, você também pode usar a função `Exception` para criar e imprimir mensagens de erro personalizadas. Aqui está um exemplo:

```Python
try:
  num = int(input("Digite um número: "))
  if num < 0:
    raise Exception("O número deve ser positivo.")
except Exception as e:
  print(e, file=sys.stderr)
```

Isso criará uma exceção personalizada com a mensagem "O número deve ser positivo." se o usuário inserir um número negativo.

## Veja também

- [Documentação da função `print`](https://docs.python.org/3/library/functions.html#print)
- [Documentação do objeto `Exception`](https://docs.python.org/3/tutorial/errors.html)