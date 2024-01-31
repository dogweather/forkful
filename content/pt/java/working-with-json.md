---
title:                "Trabalhando com JSON"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-json.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Trabalhar com JSON significa manipular o formato de dados "JavaScript Object Notation", comum para troca de informações. Programadores usam-no pela simplicidade, legibilidade e ampla aceitação em APIs e aplicações web.

## Como Fazer:
```Java
import org.json.JSONObject;

public class JsonExample {
    public static void main(String[] args) {
        // Criar um objeto JSON
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("nome", "João");
        jsonObject.put("idade", 30);
        jsonObject.put("temPet", true);
        
        // Imprimir o objeto JSON
        System.out.println(jsonObject.toString());
        
        // Ler dados do objeto JSON
        String nome = jsonObject.getString("nome");
        System.out.println("Nome: " + nome);
    }
}
```
Saída de Exemplo:
```
{"temPet":true,"nome":"João","idade":30}
Nome: João
```

## Mergulho Profundo
JSON surgiu dos padrões JavaScript, mas hoje é uma linguagem independente. Alternativas populares incluem XML e YAML, mas JSON prevalece em REST APIs pela simplicidade. Em Java, o `org.json` é um pacote comum para lidar com JSON, mas existem outras bibliotecas, como Gson e Jackson, que oferecem mais funcionalidades e eficiência.

## Veja Também
- Documentação Oficial JSON: https://www.json.org/json-pt.html
- Biblioteca Gson do Google: https://github.com/google/gson
- Biblioteca Jackson: https://github.com/FasterXML/jackson
- Especificação e documentação da API Java para JSON Processing: https://javaee.github.io/jsonp/
