---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:16.540777-07:00
description: "Como fazer: Vamos arrega\xE7ar as mangas e come\xE7ar a codificar com\
  \ JSON em Java. Primeira coisa, voc\xEA precisar\xE1 de uma biblioteca de processamento\
  \ JSON como\u2026"
lastmod: '2024-03-13T22:44:46.478032-06:00'
model: gpt-4-0125-preview
summary: "Vamos arrega\xE7ar as mangas e come\xE7ar a codificar com JSON em Java."
title: Trabalhando com JSON
weight: 38
---

## Como fazer:
Vamos arregaçar as mangas e começar a codificar com JSON em Java.

Primeira coisa, você precisará de uma biblioteca de processamento JSON como `Jackson` ou `Google Gson`. Aqui usaremos `Jackson`, então adicione essa dependência ao seu `pom.xml`:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Agora, vamos serializar (escrever) um simples objeto Java para JSON:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class ExemploJson {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Pessoa pessoa = new Pessoa("Alex", 30);
            String json = mapper.writeValueAsString(pessoa);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Pessoa {
    public String nome;
    public int idade;

    public Pessoa(String nome, int idade) {
        this.nome = nome;
        this.idade = idade;
    }
}
```

A saída deve ser:

```json
{"nome":"Alex","idade":30}
```

Agora, para desserializar (ler) JSON de volta para um objeto Java:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class ExemploJson {
    public static void main(String[] args) {
        String json = "{\"nome\":\"Alex\",\"idade\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Pessoa pessoa = mapper.readValue(json, Pessoa.class);
            System.out.println(pessoa.nome + " tem " + pessoa.idade + " anos.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

A saída será:

```
Alex tem 30 anos.
```

## Aprofundamento
A simplicidade e eficácia do JSON fizeram dele o padrão de facto para troca de dados na web, superando o XML. Introduzido no início dos anos 2000, JSON foi derivado do JavaScript, mas agora é suportado pela maioria das linguagens.

Alternativas ao JSON incluem XML, que é mais verboso, e formatos binários como Protocol Buffers ou MessagePack, que são menos legíveis por humanos, mas mais eficientes em tamanho e velocidade. Cada um tem seus casos de uso; a escolha depende das suas necessidades de dados específicas e contexto.

No Java, além do `Jackson` e `Gson`, temos também `JsonB` e `org.json` como outras bibliotecas para manipular JSON. Jackson oferece processamento baseado em fluxo e é conhecido pela velocidade, enquanto Gson é celebrado pela sua facilidade de uso. O JsonB faz parte do Jakarta EE, oferecendo uma abordagem mais padronizada.

Ao implementar JSON, lembre-se de tratar suas exceções adequadamente - seu código deve ser robusto contra entradas ruins. Além disso, considere as implicações de segurança do vinculação automática de dados – sempre valide suas entradas!

## Veja Também
- [Projeto Jackson](https://github.com/FasterXML/jackson)
- [Projeto Gson](https://github.com/google/gson)
- [Especificação JSON](https://www.json.org/json-pt.html)
- [Especificação JsonB](https://jakarta.ee/specifications/jsonb/)
