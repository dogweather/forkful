---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:58:30.623525-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Verificar se um diretório existe é simplesmente sondar o sistema de arquivos para confirmar a presença de um diretório. Programadores fazem isso para evitar erros ao acessar, ler ou escrever em diretórios que talvez não estejam lá.

## Como fazer:

```Ruby
require 'fileutils'

# Verifica se um diretório existe
if Dir.exist?('/caminho/para/o/diretorio')
  puts 'O diretório existe!'
else
  puts 'O diretório não existe.'
end

# Saída esperada:
# Quando o diretório existe: "O diretório existe!"
# Quando o diretório não existe: "O diretório não existe."
```

## Mergulho Profundo

Nos primórdios do Ruby, o acesso ao sistema de arquivos era mais verboso e menos intuitivo. Com o tempo, métodos como `Dir.exist?` foram introduzidos para facilitar essas operações. Existem alternativas, como `File.directory?`, que também podem ser usadas para verificar a existência de diretórios. No entanto, `Dir.exist?` é preferível para diretórios devido à sua semântica clara.

Em termos de implementação, essas verificações são feitas através de chamadas ao sistema operacional subjacente. Isso significa que a performance e o comportamento podem variar ligeiramente entre diferentes ambientes.

## Veja Também

- [Documentação Ruby para a classe Dir](https://ruby-doc.org/core-3.1.0/Dir.html)
- [Stack Overflow - Checando se um diretório existe com Ruby](https://stackoverflow.com/questions/1755665/check-if-a-directory-exists-in-a-ruby-script)
- [FileUtils - Módulo para operações de diretório avançadas em Ruby](https://ruby-doc.org/stdlib-3.1.0/libdoc/fileutils/rdoc/FileUtils.html)
