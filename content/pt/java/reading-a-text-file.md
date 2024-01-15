---
title:                "Lendo um arquivo de texto"
html_title:           "Java: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Java?

Se você está trabalhando com Java, é provável que precise ler dados de um arquivo de texto em algum momento. Isso pode ser necessário para processar informações armazenadas em um documento de texto ou para importar dados de um arquivo externo para o seu aplicativo Java. Em qualquer caso, ler um arquivo de texto é uma habilidade fundamental para qualquer programador Java.

## Como fazer isso:

Para ler um arquivo de texto em Java, você precisará seguir os seguintes passos:

1. Criar um objeto de arquivo usando o caminho do arquivo:
```Java
File file = new File("caminho/do/arquivo.txt");
```
2. Criar um objeto Scanner para ler o arquivo:
```Java
Scanner scanner = new Scanner(file);
```
3. Usar o método `hasNextLine()` do objeto Scanner para verificar se ainda existem linhas para ler:
```Java
while (scanner.hasNextLine()) {
	//fazer algo com a linha
}
```
4. Dentro do loop, você pode usar o método `nextLine()` para ler cada linha do arquivo:
```Java
while (scanner.hasNextLine()) {
	String line = scanner.nextLine();
	//fazer algo com a linha
}
```
5. Lembre-se de fechar o objeto Scanner depois de concluir a leitura:
```Java
scanner.close();
```

Aqui está um exemplo de um programa Java completo que lê um arquivo de texto e imprime seu conteúdo:

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class ReadingTextFile {

	public static void main(String[] args) {
		//Criar objeto de arquivo
		File file = new File("caminho/do/arquivo.txt");
		
		try {
			//Criar objeto Scanner
			Scanner scanner = new Scanner(file);
			
			//Loop para ler cada linha
			while (scanner.hasNextLine()) {
				String line = scanner.nextLine();
				//Imprimir linha no console
				System.out.println(line);
			}
			
			//Fechar objeto Scanner
			scanner.close();
			
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}

	}

}
```

A saída do programa será o conteúdo do arquivo de texto impresso no console.

## Investigação detalhada

Ao ler um arquivo de texto em Java, é importante estar ciente de algumas coisas extras:

- O caminho do arquivo pode ser relativo ao diretório atual ou absoluto. Você pode usar `System.getProperty("user.dir")` para obter o diretório atual do seu projeto.
- Você pode usar o método `useDelimiter()` do objeto Scanner para especificar um caractere de delimitação entre as informações do arquivo. O valor padrão é o caractere de nova linha `\n`.
- Se o arquivo for grande, é aconselhável usar o método `hasNext()` em vez de `hasNextLine()` para verificar a existência de algo para ler.
- É recomendável usar o bloco `try-catch` quando lidar com objetos `File` e `Scanner`, pois eles podem lançar exceções.

## Veja também

- Documentação oficial da classe File em Java: https://docs.oracle.com/javase/10/docs/api/java/io/File.html
- Documentação oficial da classe Scanner em Java: https://docs.oracle.com/javase/10/docs/api/java/util/Scanner.html
- Tutorial sobre como ler e gravar arquivos em Java: https://www.baeldung.com/reading-file-in-java