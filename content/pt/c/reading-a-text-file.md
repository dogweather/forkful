---
title:    "C: Lendo um arquivo de texto."
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Por que ler um arquivo de texto em C?

Ler um arquivo de texto é uma tarefa comum em programação, especialmente em linguagens de programação como C que são amplamente usadas para manipular e processar dados. Quando se trata de trabalhar com arquivos de texto, é importante saber como ler e escrever neles. Nesse artigo, vamos nos aprofundar sobre como ler um arquivo de texto usando a linguagem de programação C.

## Como ler um arquivo de texto em C

Para ler um arquivo de texto em C, primeiro precisamos abrir o arquivo usando a função `fopen()`. Em seguida, usamos a função `fgets()` para ler linha por linha do arquivo e imprimir seu conteúdo usando `printf()`. Veja um exemplo de código abaixo:

```
#include <stdio.h>

int main() {
    FILE *arquivo;
    char linha[256];

    // abre o arquivo para leitura
    arquivo = fopen("arquivo.txt", "r");

    // verifica se o arquivo foi aberto com sucesso
    if (arquivo == NULL) {
        printf("Erro ao abrir o arquivo.");
        return 1;
    }

    // lê o arquivo linha por linha e imprime seu conteúdo
    while (fgets(linha, sizeof(linha), arquivo)) {
        printf("%s", linha);
    }

    // fecha o arquivo
    fclose(arquivo);

    return 0;
}
```

Ao executar esse código, o conteúdo do arquivo de texto será impresso no console.

## Aprofundando-se na leitura de arquivos de texto

Além da função `fgets()`, também podemos usar outras funções para ler um arquivo de texto em C, como `fscanf()` e `getc()`. A escolha da função a ser usada depende do objetivo ou tipo de dado que desejamos ler do arquivo. Além disso, também é importante lembrar de fechar o arquivo após a leitura usando `fclose()`.

Outro aspecto importante é o modo de abertura do arquivo ao usar `fopen()`. Existem diferentes modos que podemos especificar, dependendo se queremos apenas ler o arquivo, escrever no arquivo ou ambos. Além disso, também é possível manipular o ponteiro de posição dentro do arquivo usando funções como `fseek()` e `ftell()`.

Ler arquivos de texto em C é uma habilidade essencial para qualquer programador, pois permite trabalhar com dados em formato de texto, que é amplamente utilizado em aplicações do dia a dia. Com esse conhecimento, podemos construir programas que leem, processam e geram informações a partir de arquivos de texto.

# Veja também

- [Como escrever em um arquivo de texto em C](https://www.example.com/como-escrever-em-um-arquivo-de-texto-em-c)
- [Documentação oficial da função `fopen()`](https://www.example.com/doci-funcao-fopen-c)