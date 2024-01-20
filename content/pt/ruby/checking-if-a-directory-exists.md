---
title:                "Verificando se um diretório existe"
html_title:           "Kotlin: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Verificando se o diretório existe em Ruby
Neste artigo, vamos aprender sobre como verificar se um diretório existe em Ruby.

## O Que & Por Quê?
Verificar se um diretório existe é uma prática importante ao lidar com arquivos em sua codificação. É vital para evitar erros e garantir que seu aplicativo esteja funcionando corretamente ao lidar com diretórios e arquivos.

## Como Fazer:
Em Ruby, verificar se um diretório existe é bastante simples com a ajuda do módulo `File` e seu método `directory?` ou `exist?`. Aqui estão alguns exemplos:

```Ruby
# Verificar se um diretório existe usando File.directory?
if File.directory?("/caminho/do/diretorio")
  puts "O diretorio existe"
else
  puts "O diretorio nao existe"
end

# Verificar se um diretório existe usando File.exist?
if File.exist?("/caminho/do/diretorio")
  puts "O diretorio existe"
else
  puts "O diretorio nao existe"
end
```
Quando você executa o código acima, verá a saída "O diretório existe" se o diretório especificado existir e "O diretório não existe" se não for.

## Mergulho Profundo:
Historicamente, a necessidade de verificar a existência de diretórios ou arquivos decorre do fato de que os programas podem falhar se tentarem acessar recursos que não estão disponíveis.

O método `File.directory?` valida se o caminho fornecido é um diretório. Da mesma forma, `File.exist?` verifica a existência de um arquivo ou diretório. Se você precisar verificar a existência apenas de um arquivo, pode usar `File.file?(nome_do_arquivo)`.

Alternativamente, você pode usar a gem do Ruby chamada `FileUtils`. Ela é um pacote de ferramentas que permite manipular diretórios de maneira muito mais diversificada.

Quando o método `File.directory?` ou `File.exist?` é chamado, a máquina faz um 'syscall' para verificar fisicamente a existência do diretório ou arquivo. Isso significa que pode ter um tempo de execução variado dependendo de onde o diretório está localizado (por exemplo, em um disco local ou em uma unidade de rede).

## Veja Também:
Para mais informações e recursos de aprendizado sobre manipulação de arquivos e diretórios em Ruby, confira os seguintes links:

- Documentação oficial do Ruby: [Classe File](https://ruby-doc.org/core-3.0.0/File.html)
- Documentação oficial do Ruby: [Módulo FileUtils](https://ruby-doc.org/stdlib-2.5.3/libdoc/fileutils/rdoc/FileUtils.html)
- [Dicas e truques para manipulação de arquivos Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)