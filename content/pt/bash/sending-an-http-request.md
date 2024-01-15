---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que

Se você está desenvolvendo um aplicativo web ou precisa automatizar tarefas em seu sistema, enviar uma solicitação HTTP é uma habilidade essencial. Isso permite que você obtenha informações de outros servidores e integre diferentes sistemas de forma eficiente.

## Como Fazer

Para enviar uma solicitação HTTP em Bash, usamos o comando `curl`. Vamos dar uma olhada em alguns exemplos e como interpretar sua saída.

```Bash
# Enviando uma solicitação GET para o site da Google
curl https://www.google.com

# Saída:
# <HTML><HEAD><meta http-equiv="content-type" content="text/html;charset=utf-8">
# ...


# Enviando uma solicitação POST com parâmetros
curl -d "username=example&password=1234" https://www.exemplo.com/login

# Saída:
# {"success":true,"message":"Login realizado com sucesso!"}
```

Aqui, usamos a opção `-d` para enviar dados no formato `username=example&password=1234`, que é usado em formulários da web. A saída retorna um JSON com uma mensagem de sucesso.

## Mergulho Profundo

Além do `curl`, também podemos usar o utilitário `wget` para enviar solicitações HTTP em Bash. Ambos os comandos têm algumas opções úteis para nos ajudar a personalizar nossas solicitações, como adicionar cabeçalhos personalizados, definir o tipo de requisição e salvar a resposta em um arquivo.

Por exemplo, se quisermos fazer o download de um arquivo de uma URL específica, podemos usar o `wget` da seguinte forma:

```Bash
wget -O arquivo.html http://www.exemplo.com/pagina.html

# O parâmetro -O permite definir o nome do arquivo na sua máquina
```

Podemos combinar o uso do `curl` e `grep` em Bash para fazer uma consulta no site Who Is. Por exemplo, para procurar quem é o dono de um domínio específico, podemos usar o seguinte comando:

```Bash
curl http://www.whois.com/whois/urlexemplo.com | grep -i "Registrant"
# O parâmetro -i permite ignorar a diferença entre letras maiúsculas e minúsculas
```

Este comando irá enviar uma solicitação para a página Who Is do domínio `urlexemplo.com` e filtrar a saída para mostrar apenas as informações do proprietário.

## Veja Também

- [Documentação do `curl`](https://curl.haxx.se/docs/)
- [Guia rápido do `wget`](https://www.gnu.org/software/wget/manual/wget.pdf)
- [Comandos básicos do Bash para iniciantes](https://www.tecmint.com/useful-linux-basic-commands-for-beginners/)