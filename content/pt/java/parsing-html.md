---
title:                "Java: Interpretando HTML"
simple_title:         "Interpretando HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/parsing-html.md"
---

{{< edit_this_page >}}

##Por que: 
O HTML é a linguagem de marcação utilizada para criar páginas da web. Com o aumento constante do número de páginas e sites, torna-se necessário extrair informações específicas desses sites para diversas finalidades, como análise de dados ou implementação de web scraping. Nesses casos, o parsing (análise) do HTML é uma técnica essencial para obter os dados desejados de forma eficiente.

##Como fazer:
Para realizar o parsing de HTML em Java, é necessário utilizar uma biblioteca que forneça métodos para lidar com as tags e atributos do HTML. Uma das bibliotecas mais populares é o Jsoup, que possui uma API simples e de fácil utilização.

A seguir, apresentamos um exemplo básico de código utilizando o Jsoup para fazer o parsing de uma página HTML e extrair o título e a descrição de um artigo:

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

public class ParsingHTML {
    
    public static void main(String[] args) throws IOException {
        
        // faz a conexão com a página do artigo
        Document doc = Jsoup.connect("https://www.example.com/artigo").get();
        
        // extrai o título do artigo
        Element titulo = doc.select("h1").first();
        System.out.println("Título: " + titulo.text());
        
        // extrai a descrição do artigo
        Element descricao = doc.select("p").first();
        System.out.println("Descrição: " + descricao.text());
    }
}
```
A saída do código acima seria algo como:

> Título: Como fazer o parsing de HTML em Java
> Descrição: Aprenda a extrair dados de páginas HTML utilizando a biblioteca Jsoup.

##Deep Dive:
Existem diversas técnicas e abordagens para realizar o parsing de HTML em Java. Além do Jsoup, também é possível utilizar outras bibliotecas, como o HTML Parser e o JAXB. Cada uma delas possui suas vantagens e desvantagens, e a escolha da melhor opção dependerá das necessidades do seu projeto.

Além disso, é importante considerar a estrutura e qualidade do HTML da página que está sendo analisada. Caso a página possua uma estrutura complexa ou mal feita, pode ser mais desafiador fazer o parsing e será necessário utilizar técnicas mais avançadas, como expressões regulares.

##Veja também:
- [Tutorial do Jsoup](https://jsoup.org/cookbook/)
- [Documentação do HTML Parser](https://htmlparser.sourceforge.io/)
- [Documentação do JAXB](https://docs.oracle.com/javase/tutorial/jaxb/intro/index.html)
- [Artigo sobre expressões regulares em Java](https://www.devmedia.com.br/introducao-a-expressoes-regulares-em-java/28624)