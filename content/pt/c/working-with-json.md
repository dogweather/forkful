---
title:                "Trabalhando com JSON"
date:                  2024-01-19
simple_title:         "Trabalhando com JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-json.md"
---

{{< edit_this_page >}}

## O Que é e Por Que?

Trabalhar com JSON envolve manipular dados em um formato leve, de fácil leitura para humanos e análise por máquinas. Programadores usam JSON para transferir dados entre aplicações e serviços na web de forma padronizada.

## Como Fazer:

### Lendo JSON:

```C
#include <stdio.h>
#include <stdlib.h>
#include <json-c/json.h>

int main() {
    const char * jsonString = "{\"nome\":\"João\",\"idade\":30}";
    struct json_object *parsed_json;
    struct json_object *nome;
    struct json_object *idade;

    parsed_json = json_tokener_parse(jsonString);

    json_object_object_get_ex(parsed_json, "nome", &nome);
    json_object_object_get_ex(parsed_json, "idade", &idade);

    printf("Nome: %s\n", json_object_get_string(nome));
    printf("Idade: %d\n", json_object_get_int(idade));

    json_object_put(parsed_json);
    
    return 0;
}
```

Saída:
```
Nome: João
Idade: 30
```

### Escrevendo JSON:

```C
#include <stdio.h>
#include <json-c/json.h>

int main() {
    struct json_object *pessoa = json_object_new_object();
    struct json_object *nome = json_object_new_string("Maria");
    struct json_object *idade = json_object_new_int(25);

    json_object_object_add(pessoa, "nome", nome);
    json_object_object_add(pessoa, "idade", idade);

    printf("%s\n", json_object_to_json_string(pessoa));
  
    json_object_put(pessoa);
  
    return 0;
}
```

Saída:
```
{"nome": "Maria", "idade": 25}
```

## Aprofundando:

JSON, sigla de JavaScript Object Notation, surgiu nos anos 2000 como alternativa ao XML para a troca de dados. Enquanto o JSON é mais leve e de leitura mais fácil, o XML é mais extenso com suporte a namespaces e atributos. Em C, lidar com JSON envolve bibliotecas como `json-c` ou `Jansson`. Seus parsers convertem strings para estruturas de dados acessíveis, enquanto os serializadores fazem o inverso.
 
## Veja Também:

- Documentação da `json-c`: https://json-c.github.io/json-c/
- Tutorial sobre JSON com `Jansson`: http://www.digip.org/jansson/doc/2.7/tutorial.html
- Comparação entre JSON e XML: https://www.json.org/xml.html
