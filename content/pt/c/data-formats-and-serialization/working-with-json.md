---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:54.503022-07:00
description: "Trabalhar com JSON (JavaScript Object Notation) em C envolve analisar,\
  \ gerar e manipular estruturas de dados JSON. Programadores fazem isso para\u2026"
lastmod: '2024-03-13T22:44:47.073215-06:00'
model: gpt-4-0125-preview
summary: Trabalhar com JSON (JavaScript Object Notation) em C envolve analisar, gerar
  e manipular estruturas de dados JSON.
title: Trabalhando com JSON
weight: 38
---

## Como fazer:
Para trabalhar com JSON em C, você normalmente usará uma biblioteca como `jansson` ou `json-c`, devido à falta de suporte nativo do C para JSON. Aqui, focaremos na `jansson` por sua facilidade de uso e manutenção ativa. Primeiro, instale a biblioteca (por exemplo, usando um gerenciador de pacotes como `apt` no Ubuntu: `sudo apt-get install libjansson-dev`).

Vamos começar analisando uma string JSON e acessando seu conteúdo:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "erro: na linha %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Nome: %s\nIdade: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

Saída de exemplo:
```
Nome: John Doe
Idade: 30
```

Em seguida, criando e exibindo um objeto JSON:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

Saída de exemplo:
```
{"name": "Jane Doe", "age": 25}
```

Estes exemplos demonstram o básico de carregar uma string JSON, descompactar seus valores, criar um novo objeto JSON e, então, exibi-lo como uma string.

## Aprofundando
A necessidade de trabalhar com JSON em C surge da adoção do JSON pela web como formato primário para intercâmbio de dados. A simplicidade e eficiência do JSON o fizeram superar rapidamente o XML, apesar da ausência inicial de suporte direto do C para manipulação de JSON. Soluções iniciais envolviam manipulação manual de strings - propensas a erros e ineficientes. Bibliotecas como `jansson` e `json-c` surgiram para preencher essa lacuna, fornecendo APIs robustas para análise, construção e serialização de JSON.

Enquanto `jansson` oferece simplicidade e facilidade de uso, `json-c` pode atrair aqueles que procuram um conjunto de recursos mais amplo. No entanto, alternativas como bibliotecas de análise em C++ oferecem abstrações mais sofisticadas, graças às estruturas de dados mais complexas desse idioma e ao suporte da biblioteca padrão. No entanto, quando se trabalha em ambientes onde C é o idioma preferido ou necessário - como em sistemas embutidos ou ao interagir com bibliotecas C existentes - usar `jansson` ou `json-c` torna-se indispensável.

Também vale ressaltar que trabalhar com JSON em C envolve um entendimento mais profundo do gerenciamento de memória, já que essas bibliotecas frequentemente retornam objetos alocados dinamicamente que requerem desalocação explícita. Isso desafia os programadores a equilibrar conveniência com a responsabilidade de prevenir vazamentos de memória, um aspecto crucial na elaboração de código C eficiente.
