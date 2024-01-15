---
title:                "Lendo argumentos da linha de comando"
html_title:           "Python: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Muitos programas de computador permitem aos usuários fornecerem entradas personalizadas através da linha de comando. Aprender a ler argumentos de linha de comando em Python pode ajudá-lo a criar programas mais interativos e flexíveis.

## Como fazer

Usando o módulo `sys` em Python, podemos acessar os argumentos fornecidos pela linha de comando. Aqui está um exemplo simples:

```python
import sys

# Verifica se pelo menos um argumento foi fornecido
if len(sys.argv) > 1:
    # Imprime o primeiro argumento
    print("Olá, " + sys.argv[1] + "!")
else:
    print("Favor fornecer um nome como argumento.")
```

Para testar este código, salve-o como `argumentos.py` e execute na linha de comando com um nome como argumento:

```bash
python argumentos.py Maria
```

O programa deve imprimir "Olá, Maria!".

## Profundidade

A função `sys.argv` retorna uma lista de strings. O primeiro item da lista é sempre o nome do programa sendo executado. Qualquer argumento fornecido pela linha de comando é armazenado como uma string nos itens subsequentes da lista. Podemos usar a função `len()` para verificar quantos argumentos foram fornecidos, e acessá-los usando a indexação padrão de lista.

Veja abaixo um exemplo mais complexo que permite aos usuários fornecerem vários argumentos e também exibe uma mensagem agradável:

```python
import sys

# Verifica se pelo menos um argumento foi fornecido
if len(sys.argv) > 1:
    # Imprime uma mensagem personalizada
    print("Obrigado por fornecer {} argumento(s) da linha de comando!".format(len(sys.argv) - 1))
    # Imprime cada argumento fornecido
    print("Seus argumentos foram:")
    for argumento in sys.argv[1:]:
        print("- " + argumento)
else:
    print("Favor fornecer um ou mais argumentos.")
```

Agora, se executarmos o seguinte comando:

```bash
python argumentos.py Maria Pedro João
```

A saída será:

```
Obrigado por fornecer 3 argumentos da linha de comando!
Seus argumentos foram:
- Maria
- Pedro
- João
```

## Veja também

- Documentação oficial do módulo `sys` (em inglês): https://docs.python.org/3/library/sys.html
- Como criar e executar um arquivo Python (em português): https://www.alura.com.br/artigos/criando-e-executando-um-programa-python