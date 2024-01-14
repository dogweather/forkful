---
title:    "Bash: Extraindo subcadeias de caracteres"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que usar extração de substrings?

Extração de substrings é uma técnica importante em programação que é útil em muitas situações diferentes. Ao extrair substrings, você pode segmentar uma string maior em partes menores e trabalhar com essas partes separadamente. Isso pode economizar tempo e esforço, especialmente em situações onde você precisa lidar com informações específicas dentro de uma string.

## Como fazer

Existem várias maneiras de realizar extração de substrings em um script Bash. Aqui estão dois exemplos simples:

```Bash
# Exemplo 1
string="Hello World"
echo ${string:0:5}
```

Neste exemplo, definimos a variável `string` como "Hello World" e usamos a sintaxe `${string:posição:comprimento}` para extrair os primeiros 5 caracteres. O resultado desse comando seria "Hello".

```Bash
# Exemplo 2
string="Bom dia!"
echo ${string: -5}
```

Neste segundo exemplo, usamos a mesma sintaxe, mas desta vez especificamos uma posição negativa. Isso significa que o Bash irá contar a posição a partir do final da string. Neste caso, o resultado seria "dia!".

## Mergulho Profundo

Agora que você já sabe como extrair substrings em Bash, é importante entender alguns conceitos adicionais que podem ser úteis. Primeiro, a posição sempre começa em 0, o que significa que a primeira letra da string tem a posição 0, a segunda letra tem a posição 1 e assim por diante. Além disso, ao omitir o comprimento, o Bash irá extrair todos os caracteres a partir da posição especificada até o final da string.

Outra funcionalidade útil é a possibilidade de definir variáveis enquanto extrai substrings. Por exemplo:

```Bash
string="Olá amigo!"
substring=$(echo ${string:4:6})
echo $substring
```

Neste exemplo, primeiro nós extraímos a substring "amigo" da string "Olá amigo!" e a atribuímos à variável `substring`. Em seguida, imprimimos o valor dessa variável e o resultado seria "amigo".

## Veja Também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Tutorial sobre extração de substrings em Bash](https://www.baeldung.com/linux/bash-substring-commands)
- [Outros exemplos de extração de substrings em Bash](https://linuxize.com/post/bash-substring/)