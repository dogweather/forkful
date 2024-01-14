---
title:    "Python: Extrair Substrings"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings?

A extração de substrings é uma tarefa comum em programação Python e pode ser útil por vários motivos, como a manipulação de dados de texto ou a validação de entradas de usuário. Ao dominar essa técnica, você pode melhorar suas habilidades de programação e tornar seu código mais eficiente.

## Como fazer

Para extrair substrings em Python, podemos usar o método "substring()" ou a notação de fatiamento do Python, que permite especificar a posição inicial e final do substring desejado. Vamos dar uma olhada em alguns exemplos de código para entender melhor:

```
# Usando o método "substring()"
string = "Bem-vindo ao meu blog!"
substring = string.substring(9, 17) # A substring será extraída a partir da posição 9 até a posição 16
print(substring) # Saída: "ao meu b"

# Usando a notação de fatiamento
string = "Hello, world!"
substring = string[3:6] # A substring será extraída a partir da posição 3 até a posição 5
print(substring) # Saída: "lo," 
```

Como você pode ver nos exemplos acima, o método "substring()" requer o uso de valores numéricos para especificar as posições inicial e final, enquanto a notação de fatiamento permite usar índices negativos, que contam a partir do final da string.

## Mergulho profundo

Além dos métodos e notações mencionados, também podemos extrair substrings usando expressões regulares (regex). Isso permite que você procure um padrão específico na string e extraia a parte correspondente. Por exemplo:

```
import re
string = "Meu número de telefone é (123)456-7890."
pattern = re.compile(r'\(\d{3}\)\d{3}-\d{4}') # O padrão corresponde ao formato de um número de telefone
substring = re.search(pattern, string).group() # "search()" encontra a primeira ocorrência do padrão na string
print(substring) # Saída: "(123)456-7890"
```

Você também pode usar métodos de string adicionais, como "find()" ou "index()", para encontrar a posição de um determinado caractere ou substring, e, em seguida, usar a notação de fatiamento para extrair a parte desejada da string.

## Veja também

- [Documentação oficial do Python sobre strings](https://docs.python.org/3/library/stdtypes.html#str)
- [Documentação oficial do Python sobre expressões regulares](https://docs.python.org/3/howto/regex.html)
- [Outro artigo sobre extração de substrings em Python](https://www.geeksforgeeks.org/python-ways-to-extract-number-from-string/)

Espero que este artigo tenha ajudado você a entender melhor como extrair substrings em Python. Agora você pode aplicar esse conhecimento em seus próprios projetos e melhorar suas habilidades de programação. Obrigado por ler e até a próxima!