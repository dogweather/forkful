---
title:                "Criando um arquivo temporário"
date:                  2024-01-20T17:39:32.870833-07:00
model:                 gpt-4-1106-preview
simple_title:         "Criando um arquivo temporário"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Criar um arquivo temporário permite manipular dados sem afetar o sistema de arquivos permanente. Programadores fazem isso para testes, armazenamento temporário de informações e para garantir que dados não fiquem em disco após o uso.

## Como Fazer:
Vejamos um código em C que cria um arquivo temporário:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *temp = tmpfile();
    
    if(temp) {
        fputs("Exemplo de texto temporário.\n", temp);
        
        // Volte para o início do arquivo para ler.
        rewind(temp);
        
        char buffer[255];
        while(fgets(buffer, 255, temp) != NULL) {
            printf("%s", buffer);
        }
        
        // O arquivo temporário é removido automaticamente ao fechar.
        fclose(temp);
    } else {
        printf("Não foi possível criar o arquivo temporário.\n");
    }

    return EXIT_SUCCESS;
}
```

Resultado esperado (Se tudo der certo, claro):
```
Exemplo de texto temporário.
```

## Aprofundando

Arquivos temporários não são uma ideia nova. Surgiram como uma forma de gerenciar dados que só precisam existir durante a execução de um programa. Existem várias maneiras de criar um:

1. `tmpfile()`: abre um arquivo temporário binário que é removido automaticamente ao fechar ou ao terminar o programa.
2. `mkstemp()`: cria e abre um arquivo temporário com um nome exclusivo. Deve ser removido manualmente pelo programador.
3. Arquivos em `/tmp` (em sistemas Unix-like): ao criar arquivos aqui, eles geralmente são apagados ao reiniciar o sistema.

`tmpfile()` é fácil de usar, mas tem limitações de segurança em ambientes multiusuário. `mkstemp()`, por outro lado, é mais seguro contra ataques de link simbólico, pois garante um nome de arquivo exclusivo quando criado corretamente.

## Veja Também
- Documentação do GNU sobre arquivos temporários: https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html
- `mkstemp()` man page: https://linux.die.net/man/3/mkstemp
- C Standard Library Reference: https://en.cppreference.com/w/c/io/tmpfile

Lembre-se que detalhes extras e questões específicas sobre seu ambiente de desenvolvimento ou necessidades do projeto podem requerer um pouquinho mais de pesquisa. Só não complica mais do que precisa, combinado?
