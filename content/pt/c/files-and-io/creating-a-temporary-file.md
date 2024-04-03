---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:08.330832-07:00
description: "Criar um arquivo tempor\xE1rio em C envolve gerar um arquivo destinado\
  \ a ser usado por curta dura\xE7\xE3o, geralmente como espa\xE7o de rascunho para\
  \ processamento\u2026"
lastmod: '2024-03-13T22:44:47.071010-06:00'
model: gpt-4-0125-preview
summary: "Criar um arquivo tempor\xE1rio em C envolve gerar um arquivo destinado a\
  \ ser usado por curta dura\xE7\xE3o, geralmente como espa\xE7o de rascunho para\
  \ processamento ou armazenamento de dados."
title: "Criando um arquivo tempor\xE1rio"
weight: 21
---

## O Quê & Porquê?
Criar um arquivo temporário em C envolve gerar um arquivo destinado a ser usado por curta duração, geralmente como espaço de rascunho para processamento ou armazenamento de dados. Programadores fazem isso para gerenciar dados temporários sem afetar o armazenamento permanente do programa ou para garantir que dados sensíveis sejam apagados após o uso.

## Como fazer:
Criar um arquivo temporário na linguagem de programação C pode aproveitar funções como `tmpfile()` e `mkstemp()`.

**Usando `tmpfile()`**: Esta função cria um arquivo temporário único que é automaticamente deletado quando o programa termina ou o arquivo é fechado.

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("Falha ao criar arquivo temporário");
        return 1;
    }

    // Escrevendo dados no arquivo temporário
    fputs("Isto é um teste.\n", temp);

    // Voltar e ler o que escrevemos
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // Deletado automaticamente ao fechar ou sair do programa
    fclose(temp);

    return 0;
}
```
**Saída de exemplo:**
```
Isto é um teste.
```

**Usando `mkstemp()`**: Fornece mais controle sobre a localização do arquivo temporário e suas permissões. Requer uma string de template que termina com `XXXXXX` a qual é então substituída por uma sequência única para evitar colisões de nome.

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char template[] = "/tmp/meutemp-XXXXXX";
    int fd = mkstemp(template);

    if (fd == -1) {
        perror("Falha ao criar arquivo temporário");
        return 1;
    }
    
    printf("Arquivo temporário criado: %s\n", template);

    // Arquivos temporários criados com mkstemp() devem ser deletados manualmente
    unlink(template);

    close(fd);
    return 0;
}
```
**Saída de exemplo:**
```
Arquivo temporário criado: /tmp/meutemp-abc123
```

## Aprofundando
O conceito de arquivos temporários não é exclusivo para C, mas é uma funcionalidade comum em muitos ambientes de programação devido à sua utilidade no manuseio de dados efêmeros. A função `tmpfile()`, padronizada na norma ISO C, cria um arquivo com um nome único em um diretório padrão, mas sua existência é breve, tornando-o ideal para operações seguras ou temporárias.

Uma limitação notável de `tmpfile()` é sua dependência do diretório temporário padrão, o que pode não ser adequado para todas as aplicações, especialmente em termos de permissões ou segurança. Em contraste, `mkstemp()` permite especificar o diretório e garante a criação segura de arquivos com nomes únicos modificando a string de template fornecida, oferecendo uma solução mais versátil à custa de gerenciamento manual de arquivos.

No entanto, a criação de arquivos temporários pode introduzir vulnerabilidades de segurança, como condições de corrida, se não for manuseada corretamente. Por exemplo, `tmpfile()` e `mkstemp()` abordam diferentes aspectos da criação segura de arquivos temporários (deleção automática e geração de nome seguro, respectivamente), mas nenhum deles é uma solução completa. Os desenvolvedores devem considerar as especificidades das necessidades de segurança de sua aplicação, incluindo potenciais vulnerabilidades introduzidas por arquivos temporários, e podem precisar implementar salvaguardas adicionais além do que essas funções fornecem.

No panorama mais amplo da programação, alternativas como armazenamento em memória (por exemplo, usando estruturas de dados dinâmicas ou arquivos mapeados em memória) podem oferecer melhor desempenho ou segurança para o manuseio de dados temporários. No entanto, arquivos temporários físicos continuam sendo uma ferramenta crucial em muitos cenários, especialmente para conjuntos de dados grandes ou quando a comunicação entre processos está envolvida.
