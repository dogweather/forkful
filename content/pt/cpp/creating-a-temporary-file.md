---
title:    "C++: Criando um arquivo temporário"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que
Você já se perguntou por que às vezes precisamos criar arquivos temporários ao escrever um programa em C++? Bem, é uma ferramenta útil para armazenar dados temporários que podem ser acessados ​​e manipulados durante a execução do programa. Vamos dar uma olhada em como podemos fazer isso de maneira eficiente.

## Como fazer
Para criar um arquivo temporário, podemos usar a função `tmpfile()` da biblioteca padrão `<cstdio>`. Vamos dar uma olhada em um exemplo simples:

```C++
#include <cstdio>

int main() {
    // Crie um ponteiro de arquivo temporário
    FILE *temp_file = tmpfile();
    if (temp_file != NULL) {
        // Escreva dados no arquivo
        fprintf(temp_file, "Olá, mundo!");

        // Feche o arquivo temporário
        fclose(temp_file);
    }
    return 0;
}
```
Se executarmos este programa, veremos que um arquivo temporário foi criado no diretório de trabalho atual com o texto "Olá, mundo!" dentro dele.

## Mergulho profundo
A função `tmpfile()` cria um arquivo temporário em modo de leitura e gravação, com o arquivo aberto em modo binário. Isso significa que podemos escrever qualquer tipo de dados no arquivo, desde que usemos o `fprintf()` para formatá-los corretamente. Além disso, o arquivo é excluído automaticamente quando o programa é encerrado ou quando usamos a função `fclose()` para fechá-lo explicitamente.

Também é importante lembrar que o arquivo temporário é criado em um local padrão do sistema operacional, o que pode variar dependendo do sistema em que estamos executando o programa. Portanto, é uma boa prática usar a função `tmpnam()` para obter o caminho completo do arquivo temporário antes de criá-lo.

## Veja também
- [Documentação da função `tmpfile()`](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [Tutorial básico de C++](https://www.cplusplus.com/doc/tutorial/)
- [Referência rápida do C++](https://www.cplusplus.com/files/tutorial.pdf)