---
title:                "Criando um arquivo temporário"
html_title:           "Java: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# O que & Por quê?

Criar um arquivo temporário é um processo comum em programação onde um arquivo é criado temporariamente durante a execução de um programa. Isso é feito para armazenar dados temporários que serão utilizados em alguma etapa do código.

Os programadores criam arquivos temporários para diversas finalidades, como por exemplo, armazenar dados temporários de um usuário durante o processo de autenticação, salvar informações temporárias durante a execução de um algoritmo complexo ou mesmo para criar backups de arquivos que serão modificados durante a execução do programa.

# Como fazer:

````Java
// Exemplo de criação de um arquivo temporário
File tempFile = File.createTempFile("exemplo", ".txt");
// Define que o arquivo será deletado quando o programa terminar de executar
tempFile.deleteOnExit();

// Escreve dados no arquivo
try (BufferedWriter writer = new BufferedWriter(new FileWriter(tempFile))) {
    writer.write("Este é um exemplo de arquivo temporário criado em Java.");
    writer.newLine();
    writer.write("Será deletado quando o programa terminar de executar.");
}
catch (IOException e) {
    // Trata a exceção
    System.out.println("Erro ao escrever no arquivo temporário.");
}

// Lê os dados do arquivo
try (BufferedReader reader = new BufferedReader(new FileReader(tempFile))) {
    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println(line);
    }
}
catch (IOException e) {
    // Trata a exceção
    System.out.println("Erro ao ler arquivo temporário.");
}

// Saída:
// Este é um exemplo de arquivo temporário criado em Java.
// Será deletado quando o programa terminar de executar.
````

# Profundando:

Criar arquivos temporários é uma prática comum na programação desde a década de 1980, quando surgiram as primeiras linguagens de programação com suporte a arquivos. Antes disso, os programadores precisavam lidar com o gerenciamento manual da memória e o espaço em disco era limitado, então a criação de arquivos temporários era menos comum.

Além da forma apresentada acima, existem outras maneiras de criar arquivos temporários em Java, como utilizar a classe `TemporaryFilesystem` do framework Apache Commons IO ou a classe `TempFile` da biblioteca Guava.

É importante ressaltar que ao criar um arquivo temporário, é necessário definir o seu comportamento após a execução do programa. Caso não seja especificado, o arquivo será mantido e pode ocupar espaço desnecessariamente no sistema.

# Veja também:

- [Documentação oficial do Java sobre criação de arquivos temporários](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-)
- [Classe TemporaryFilesystem do Apache Commons IO](https://commons.apache.org/proper/commons-io/javadocs/api-2.6/org/apache/commons/io/file/TemporaryFilesystem.html)
- [Classe TempFile da biblioteca Guava](https://google.github.io/guava/releases/19.0/api/docs/com/google/common/io/TempFile.html)