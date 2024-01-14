---
title:    "Java: Escrevendo um arquivo de texto."
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto é importante na programação Java

Escrever e manipular arquivos de texto é um aspecto fundamental da programação Java. Ao aprender a criar e modificar arquivos de texto, você poderá armazenar e recuperar informações importantes em suas aplicações, tornando seus programas mais dinâmicos e eficientes.

## Como escrever um arquivo de texto em Java

Para escrever um arquivo de texto em Java, primeiro é necessário criar um objeto do tipo `File` que representará o arquivo desejado. Em seguida, utilize a classe `FileWriter` para abrir o arquivo criado e utilizar o método `write()` para inserir conteúdo no arquivo. Por fim, é importante fechar o arquivo utilizando o método `close()` para garantir que os dados sejam salvos corretamente.

Veja um exemplo de código em Java para escrever um arquivo de texto contendo uma lista de compras:

```Java
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class ArquivoDeTexto {
    public static void main(String[] args) {
        // Criando um arquivo chamado "lista_de_compras.txt"
        File listaDeCompras = new File("lista_de_compras.txt");
        
        try {
            // Abrindo o arquivo para escrita
            FileWriter writer = new FileWriter(listaDeCompras);
            
            // Escrevendo os itens da lista de compras
            writer.write("Lista de compras:\n");
            writer.write("1. Leite\n");
            writer.write("2. Ovos\n");
            writer.write("3. Pão\n");
            
            // Fechando o arquivo
            writer.close();
            
            System.out.println("Arquivo criado com sucesso!");
        } catch (IOException e) {
            System.out.println("Erro ao criar o arquivo.");
            e.printStackTrace();
        }
    }
}
```

O arquivo `lista_de_compras.txt` será criado no mesmo diretório onde o seu programa está sendo executado e terá o seguinte conteúdo:

```
Lista de compras:
1. Leite
2. Ovos
3. Pão
```

## Mergulho profundo: entendendo o processo de escrita de arquivos de texto em Java

Quando utilizamos o método `write()` da classe `FileWriter`, os dados são armazenados em buffer antes de serem escritos no arquivo físico. Isso significa que os dados são primeiramente armazenados na memória, permitindo que o processo de escrita seja mais rápido, e depois são transferidos para o arquivo. Porém, é importante utilizar o método `close()` após a escrita para garantir que os dados sejam gravados em disco antes do programa encerrar.

É importante lembrar que, ao utilizar a classe `FileWriter`, o arquivo é sobrescrito caso já exista um com o mesmo nome. Para adicionar conteúdo ao final de um arquivo existente, é necessário utilizar a classe `FileWriter` em conjunto com a classe `FileWriter` e passar `true` como segundo argumento para o construtor, indicando que os dados devem ser acrescentados e não sobrescritos.

## Veja também

- [Documentação oficial do Java sobre a classe `File`](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Tutorial da Oracle sobre como escrever arquivos em Java](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Dicas para manipular arquivos de texto em Java](https://www.devmedia.com.br/manipulando-arquivos-de-texto-com-java/19978)