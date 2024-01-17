---
title:                "Trabalhando com json"
html_title:           "C: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-json.md"
---

{{< edit_this_page >}}

O que é JSON e por que os programadores o usam?

JSON é uma linguagem de marcação de dados amplamente utilizada por programadores para armazenar e transmitir informações estruturadas. Eles usam essa linguagem por sua simplicidade, portabilidade e capacidade de trabalhar bem com outras linguagens.

Como fazer:

Existem duas maneiras principais de trabalhar com JSON em C: usando uma biblioteca ou escrevendo seu próprio analisador/gerador. Aqui está um exemplo de como usar a biblioteca Json-c para analisar e imprimir um arquivo JSON:

```C
#include <stdio.h>
#include <json-c/json.h>

int main() {

    //Ler o arquivo JSON
    FILE *fp = fopen("dados.json", "r");

    char buffer[1024];
    struct json_object *parsed_json;
    struct json_object *nome;
    struct json_object *idade;

    // Analisar o arquivo JSON e armazená-lo em um objeto
    fgets(buffer, 1024, fp);
    parsed_json = json_tokener_parse(buffer);

    // Obter os valores específicos do arquivo JSON e imprimi-los
    json_object_object_get_ex(parsed_json, "nome", &nome);
    printf("Nome: %s\n", json_object_get_string(nome));
    json_object_object_get_ex(parsed_json, "idade", &idade);
    printf("Idade: %d", json_object_get_int(idade));

    return 0;
}
```

Saída:

```
Nome: João
Idade: 25
```

Mergulho profundo:

JSON foi desenvolvido em 2001 por Douglas Crockford e é uma alternativa ao formato de dados XML mais complexo. Existem outras bibliotecas disponíveis para trabalhar com JSON em C, como a Jansson. Além disso, é possível criar seu próprio analisador/gerador de JSON para obter um controle mais preciso sobre o processo.

Veja também:

[Aprenda JSON em 10 minutos](https://www.json.org/json-pt.html)

[Json-c documentação](https://json-c.github.io/json-c/json-c-0.12.1/doc/html/index.html)

[Jansson documentação](https://jansson.readthedocs.io/en/2.12/)