---
title:    "Python: Escrevendo para o erro padrão"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão em Python?

Quando se programa em Python, é importante saber como lidar com erros. Um erro comum é o erro de sintaxe, que pode interromper a execução do programa. Escrever para o erro padrão permite que o programador visualize informações importantes sobre o erro e tome medidas necessárias para corrigi-lo.

## Como fazer:

Os seguintes exemplos de código mostram como escrever para o erro padrão em Python:

```
# Exemplo 1:
try:
  # Código que pode gerar um erro
except Exception as e:
  # Escrever mensagem de erro no erro padrão
  print("Ocorreu um erro:", e, file=sys.stderr)
  
# Exemplo 2:
# Mostrar uma mensagem de erro e encerrar o programa
print("Não é possível executar esta operação!", file=sys.stderr)
sys.exit()
```

### Resultado:

```
Não é possível executar esta operação!
```

## Mergulho profundo:

Em Python, o erro padrão é representado pelo objeto `sys.stderr`. Ao usar a função `print()` com o argumento `file=sys.stderr`, é possível imprimir uma mensagem de erro para o erro padrão. Isso é útil para informar ao usuário do programa sobre um erro e também para depurar problemas em código.

Para escrever em outros arquivos, basta substituir `sys.stderr` pelo nome ou caminho do arquivo desejado.

## Veja também:

- [Documentação oficial do Python sobre tratamento de erros](https://docs.python.org/3/tutorial/errors.html)
- [Tutorial do Real Python sobre tratamento de erros em Python](https://realpython.com/python-exceptions/)
- [Vídeo explicativo sobre tratamento de erros em Python](https://www.youtube.com/watch?v=NIWwJbo-9_8)