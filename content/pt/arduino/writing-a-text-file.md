---
title:    "Arduino: Escrevendo um arquivo de texto"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto é uma habilidade importante para programadores de Arduino, pois permite armazenar e ler dados em um formato legível para humanos. Isso pode ser útil para coletar informações de sensores, salvar configurações ou até mesmo criar logs de atividades.

## Como fazer?

Para escrever um arquivo de texto, primeiro é necessário criar uma instância da classe "File" e abri-la usando a função "open()". Em seguida, escreva o conteúdo do arquivo usando a função "println()" e, por fim, feche o arquivo usando a função "close()".

Um exemplo de código seria o seguinte:

```arduino
File myFile;

void setup() {
  // abre o arquivo para escrita
  myFile = open("arquivo.txt", FILE_WRITE);
  
  // escreve no arquivo
  myFile.println("Este é um arquivo de texto escrito pelo Arduino!");
  
  // fecha o arquivo
  myFile.close();
}

void loop() {
  // nada acontece no loop
}
```

O resultado desse código seria a criação de um arquivo de texto chamado "arquivo.txt" contendo a mensagem "Este é um arquivo de texto escrito pelo Arduino!".

## Profundidade

Além de escrever simplesmente uma mensagem, é possível incluir variáveis e dados nos arquivos de texto. Isso pode ser feito usando a função "print()" para adicionar informações em uma linha existente ou a função "write()" para escrever bytes no arquivo.

Também é possível criar arquivos em diferentes formatos, como CSV, utilizando separadores para indicar colunas ou linhas. Isso pode ser útil, por exemplo, para registrar dados de temperatura e umidade em um arquivo que posteriormente pode ser aberto e analisado por um programa de planilha.

## Veja também

- [Como ler um arquivo de texto com Arduino](https://www.arduino.cc/reference/pt/language/functions/communication/serial/readbytesuntil/)
- [Como trabalhar com arquivos em formato CSV em Arduino](https://www.arduino.cc/reference/pt/language/functions/file-io/open/)
- [Documentação completa sobre a classe "File" em Arduino](https://www.arduino.cc/reference/pt/language/functions/communication/serial/readstring/)