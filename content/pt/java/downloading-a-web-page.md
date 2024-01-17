---
title:                "Baixando uma página da web"
html_title:           "Java: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Baixar uma página da web significa obter todos os arquivos e conteúdo associados a uma determinada página da internet. É uma tarefa comum na programação, pois muitas vezes precisamos acessar informações online para criar aplicativos ou melhorar o processamento de dados.

## Como fazer:

```Java
import java.io.BufferedInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;

public class DownloadWebPage {
  public static void main(String[] args) throws IOException {
    // Definir a URL da página da web a ser baixada
    String url = "https://www.example.com";
 
    // Criar objeto URL e obter a conexão com a página
    URL paginaWeb = new URL(url);
    InputStream inputStream = paginaWeb.openStream();
    
    // Criar objeto BufferedInputStream para ler os dados da conexão
    BufferedInputStream buffer = new BufferedInputStream(inputStream);
 
    // Usar FileOutputStream para criar o arquivo onde os dados serão salvos
    String caminho = "pagina_web.html";
    FileOutputStream fileOutputStream = new FileOutputStream(caminho);
    
    // Ler os dados da conexão e gravá-los no arquivo
    byte[] dados = new byte[1024];
    int bytesLidos;
    while ((bytesLidos = buffer.read(dados)) != -1) {
        fileOutputStream.write(dados, 0, bytesLidos);
    }
    
    // Fechar as conexões e fluxos de dados
    fileOutputStream.close();
    buffer.close();
    inputStream.close();

    System.out.println("Página da web salva com sucesso em " + caminho);
  }
}
```

Output:
```
Página da web salva com sucesso em pagina_web.html
```

## Aprofundando:

1) Baixar uma página da web é uma técnica antiga que data dos primeiros anos da internet. Nos primórdios, as páginas eram apenas texto e imagens estáticas, por isso, o processo era bem simples. Com o desenvolvimento da internet e a inclusão de tecnologias mais avançadas, como JavaScript e CSS, baixar uma página da web se tornou uma tarefa mais complexa, pois é necessário entender como cada elemento se comporta para obter uma cópia fiel da página.

2) Além do Java, existem outras linguagens e ferramentas que podem ser usadas para baixar uma página da web, como Python, Ruby e o popular programa wget. Cada uma tem suas particularidades, mas o princípio básico é o mesmo.

3) Para baixar uma página da web com mais precisão, é possível usar bibliotecas como o Apache HttpClient ou o JSoup, especificamente criadas para lidar com requisições HTTP e análise de documentos HTML, respectivamente. Essas ferramentas facilitam o processo e permitem um maior controle sobre quais dados serão baixados.

## Veja também:

- [Tutorial: Como baixar páginas da web com Java](https://docs.oracle.com/javase/tutorial/networking/urls/readingURL.html)
- [Exemplos de código para download de páginas web em Java](https://www.tutorialspoint.com/javaexamples/internet_download.htm)
- [JSoup - biblioteca Java para análise e manipulação de HTML](https://jsoup.org/)