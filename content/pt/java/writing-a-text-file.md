---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Escrever um arquivo de texto é basicamente gravar dados em um documento que podemos ler e editar. Programadores fazem isso para salvar configurações, resultados ou para exportar dados para que outros sistemas e usuários possam acessá-los.

## Como Fazer:
```Java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class EscritorDeArquivo {
    public static void main(String[] args) {
        String caminhoDoArquivo = "exemplo.txt";
        String conteudo = "Olá, este é um texto de exemplo!";
        
        try (BufferedWriter escritor = new BufferedWriter(new FileWriter(caminhoDoArquivo))) {
            escritor.write(conteudo);
            System.out.println("Arquivo escrito com sucesso!");
        } catch (IOException e) {
            System.err.println("Ocorreu um erro ao escrever o arquivo: " + e.getMessage());
        }
    }
}
```
Saída esperada: `Arquivo escrito com sucesso!`

## Mergulho Profundo:
No passado, Java usava `FileWriter` de maneira direta, bem menos eficientes em termos de performance comparado ao padrão `BufferedWriter` de hoje. Alternativas modernas incluem o uso de `Files`, parte do NIO.2 (New I/O 2), que veio com a versão 7 do Java, permitindo escrita de arquivos com menos código. Por detalhes de implementação, usar `try-with-resources`, como no exemplo acima, garante que recursos são fechados adequadamente após uso, evitando vazamentos de memória.

## Veja Também:
- [Documentação Oficial do BufferedWriter](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/BufferedWriter.html)
- [Tutorial Oracle sobre I/O de arquivos](https://docs.oracle.com/javase/tutorial/essential/io/)
- [Diferenças entre FileWriter e BufferedWriter](https://stackoverflow.com/questions/9648811/specific-difference-between-bufferedwriter-and-filewriter)
