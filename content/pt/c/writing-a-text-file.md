---
title:    "C: Escrevendo um arquivo de texto"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto no C?

Se você está aprendendo a programar em C, provavelmente já se deparou com a necessidade de escrever um arquivo de texto em algum momento. Mas por que é importante saber como fazer isso? Bem, arquivos de texto são uma forma de armazenar dados de forma persistente em um dispositivo de armazenamento, como um disco rígido ou uma unidade de armazenamento flash. Isso significa que os dados permanecem intactos mesmo após o encerramento do programa, permitindo que você os leia e manipule posteriormente. Além disso, arquivos de texto são amplamente utilizados em aplicações do mundo real, tornando-se uma habilidade valiosa para qualquer programador C.

## Como escrever um arquivo de texto em C

Escrever um arquivo de texto em C é um processo relativamente simples, que pode ser dividido em cinco etapas principais:

1. Declarar e abrir o arquivo: primeiro, é preciso declarar um ponteiro de arquivo e então abrir o arquivo usando a função `fopen()`. É importante especificar o modo do arquivo (leitura, escrita, etc.) e o nome do arquivo desejado.

2. Escrever no arquivo: agora que o arquivo está aberto, você pode escrever nele usando a função `fprintf()`. Esta função possui uma sintaxe semelhante à função `printf()`, com a adição de um primeiro argumento que é o ponteiro do arquivo.

3. Fechar o arquivo: após terminar de escrever, é importante fechar o arquivo usando a função `fclose()`. Isso garante que todos os dados sejam salvos corretamente no arquivo.

4. Verificar erros: durante o processo de escrita, é importante verificar se ocorreram erros durante a abertura, escrita ou fechamento do arquivo. Para isso, você pode usar a função `ferror()`.

5. Manipular dados do arquivo: depois de escrever o arquivo, você pode manipular os dados lidos a partir dele usando funções como `fseek()` e `fread()`.

Aqui está um exemplo simples de como escrever um arquivo de texto em C:

```C
#include <stdio.h>

int main() {
  // declara e abre o arquivo
  FILE *arquivo = fopen("meu_arquivo.txt", "w");

  // escreve no arquivo
  fprintf(arquivo, "Olá, mundo!");

  // fecha o arquivo
  fclose(arquivo);

  // verifica erros
  if (ferror(arquivo)) {
    printf("Erro ao escrever no arquivo.");
  }

  return 0;
}
```

## Profundidade da escrita de arquivos de texto

Agora que você já sabe como escrever um arquivo de texto em C, vamos nos aprofundar um pouco mais. Além do modo de escrita "w", que é usado no exemplo acima, existem outros modos que podem ser usados para escrever em arquivos:

- Modo "r": somente leitura
- Modo "a": acrescenta dados ao final do arquivo
- Modo "w+": leitura e escrita, apaga o conteúdo pré-existente do arquivo
- Modo "a+": leitura e escrita, acrescenta dados ao final do arquivo

Além disso, é importante lembrar que diferentes sistemas operacionais têm diferentes representações para quebra de linha em arquivos de texto. Para garantir que suas quebras de linha sejam interpretadas corretamente em diferentes sistemas, é recomendável usar as constantes `fputs('\n', arquivo)` ou `fputc('\n', arquivo)` para inserir uma quebra de linha no final de cada linha de texto.

Agora você tem o conhecimento necessário para escrever arquivos de texto em C com facilidade! Pratique esses conceitos e explore outras possibilidades de escrita de arquivos para aprimorar suas habilidades como programador C.

## Veja também

- [Leitura de arquivos em C](https://exercism.io/tracks/c/exercises/cat-and-mouse)
- [Manipulação de arquivos em C](https://www.programiz.com/c-programming/c-file-input-output)