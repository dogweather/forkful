---
title:                "Trabalhando com json"
html_title:           "Java: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-json.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com JSON é uma maneira popular de armazenar e transmitir dados entre sistemas. JSON, que significa JavaScript Object Notation, é um formato de texto simples que é fácil de entender e ler para humanos e máquinas. Os programadores usam JSON porque é eficiente, flexível e amplamente suportado por várias linguagens de programação.

## Como fazer:

Para trabalhar com JSON em Java, primeiro precisamos importar a biblioteca JSON simples chamada "json-simple". Em seguida, podemos usar os métodos "parse" e "toJSONString" para converter um objeto Java em uma string JSON e vice-versa. Aqui está um exemplo para ilustrar melhor:

```Java
// Importando a biblioteca
import org.json.simple.JSONObject;
// Criando um objeto Java
JSONObject person = new JSONObject();
person.put("name", "João");
person.put("age", 30);
// Convertendo para string JSON
String json = person.toJSONString();
// Imprimindo a string JSON
System.out.println(json); // {"name": "João", "age": 30}
```

## Mergulho profundo:

JSON foi criado em 2001 por Douglas Crockford e se tornou popular devido à sua simplicidade e legibilidade. Existem algumas alternativas para trabalhar com dados estruturados, como XML e YAML, mas JSON é amplamente adotado na comunidade de desenvolvimento devido à sua integração perfeita com JavaScript e sua capacidade de ser lido e compreendido facilmente por humanos e máquinas. Além disso, existem muitas bibliotecas e ferramentas que tornam mais fácil trabalhar com JSON em várias linguagens de programação.

## Veja também:

- [Documentação oficial do JSON](https://www.json.org/json-en.html)
- [Tutorial em vídeo sobre trabalhar com JSON em Java](https://www.youtube.com/watch?v=O5nskjZ_GoI)
- [Biblioteca "json-simple" no GitHub](https://github.com/fangyidong/json-simple)