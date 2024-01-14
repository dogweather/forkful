---
title:    "C: Criando um arquivo temporário"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Por que criar um arquivo temporário em C?

A criação de arquivos temporários é frequentemente necessária durante o desenvolvimento de programas em C. Esses arquivos são úteis para armazenar dados temporários durante a execução do programa e, em seguida, excluí-los quando não forem mais necessários. Eles também podem ser usados para armazenar informações que precisam ser compartilhadas entre diferentes partes do código.

Como criar um arquivo temporário em C

Para criar um arquivo temporário em C, você precisará incluir a biblioteca `stdio.h` e usar a função `tmpfile()`. Vamos ver um exemplo:

```C
#include <stdio.h>

int main() {
    // cria um arquivo temporário
    FILE *temp_file = tmpfile();
    
    // checa se o arquivo foi criado com sucesso
    if (temp_file == NULL) {
        printf("Erro ao criar arquivo temporário.");
        return 1;
    }
    
    // escreve "Hello world" no arquivo
    fprintf(temp_file, "Hello world!");
    
    // fecha o arquivo e libera a memória
    fclose(temp_file);
    
    printf("Arquivo temporário criado com sucesso.");
}
```

A saída deste programa será:

```
Arquivo temporário criado com sucesso.
```

Este é apenas um exemplo simples de como criar um arquivo temporário em C. Você pode personalizá-lo para suas necessidades específicas.

Mergulho profundo

Ao usar a função `tmpfile()`, o sistema operacional cria um arquivo temporário e retorna um ponteiro para ele. Esse arquivo é automaticamente excluído quando é fechado ou quando o programa termina sua execução. No entanto, se você precisar manter o arquivo por mais tempo, pode usar a função `tmpnam()` para gerar um nome único para o arquivo e, em seguida, usar a função `fopen()` para criar um ponteiro de arquivo e escrever nele.

```C
#include <stdio.h>

int main() {
    // gera um nome único para o arquivo
    char *file_name = tmpnam(NULL);
    
    // cria um arquivo com o nome gerado
    FILE *temp_file = fopen(file_name, "w+");
    
    // escreve "Hello world" no arquivo
    fprintf(temp_file, "Hello world!");
    
    // fecha o arquivo
    fclose(temp_file);
}
```

Observe que, neste exemplo, o arquivo não será excluído automaticamente e você precisará excluir manualmente quando terminar de usá-lo.

Veja também

- Documentação da função `tmpfile()` em C: https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm
- Tutorial sobre criação de arquivos temporários em C: https://www.geeksforgeeks.org/temporary-files-creating-using-c/