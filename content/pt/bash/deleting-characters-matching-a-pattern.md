---
title:    "Bash: Excluir caracteres que correspondam a um padrão"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que

Às vezes, ao escrever um script em Bash, você pode precisar remover caracteres que correspondam a um determinado padrão. Isso pode ser útil para limpar entradas de texto ou para manipular strings em um formato específico. Neste artigo, vamos explorar como utilizar essa funcionalidade em seus scripts Bash.

## Como Fazer

Para excluir caracteres que correspondam a um padrão em um script Bash, podemos usar o comando `tr` (translate). Por exemplo, se quisermos remover todas as vogais de uma string, podemos usar o seguinte comando:

```Bash
tr -d 'aeiou' <<< "Hello World"
```

Isso produzirá a saída "Hll Wrld", pois ele removeu todas as vogais do texto fornecido. Podemos usar o comando `tr` para substituir os caracteres que desejamos excluir, ou até mesmo pular linhas que correspondam ao padrão. Por exemplo, se quisermos substituir todas as letras maiúsculas por asteriscos, podemos fazer o seguinte:

```Bash
tr 'A-Z' '*' <<< "Bash Programming"
```

Isso produzirá a saída "**** ***********", pois substituiu todas as letras maiúsculas por asteriscos. Para pular linhas que correspondam a um padrão, podemos usar o sinal de "circunflexo" (^). Por exemplo, se quisermos manter apenas os números de uma string, podemos fazer o seguinte:

```Bash
tr -cd '0-9\n' <<< "I have 123 apples"
```

Isso produzirá a saída "123\n", pois manteve apenas os números e pulou as letras e espaços em branco.

## Mergulho Profundo

O comando `tr` é usado para traduzir ou excluir caracteres em uma string. Ele funciona substituindo cada caractere no primeiro conjunto pelo caractere correspondente no segundo conjunto, ou excluindo-os se nenhum caractere correspondente for fornecido. O comando `tr` é sensível a maiúsculas e minúsculas, portanto, certifique-se de usar os conjuntos de caracteres corretos.

Além disso, é importante notar que o comando `tr` só pode manipular caracteres em uma única linha. Isso significa que ele não pode ser usado para manipular strings com várias linhas. Se você precisar trabalhar com várias linhas, pode considerar o uso de outras ferramentas, como o comando `sed`.

## Veja Também

Para mais informações sobre o uso do comando `tr` em scripts Bash, você pode consultar a documentação oficial do GNU `tr` em: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html.

Você também pode explorar outros comandos úteis do Bash em nosso blog, como o `grep` (localizar e imprimir) ou `sort` (classificar) para manipulação de texto. Além disso, recomendamos a leitura da documentação oficial do Bash em: https://www.gnu.org/software/bash/manual/bash.html.

Esperamos que este artigo tenha sido útil em mostrar como excluir caracteres que correspondam a um padrão em scripts Bash. Combinar comandos como o `tr` pode ser muito útil para criar scripts poderosos e eficientes. Até a próxima!