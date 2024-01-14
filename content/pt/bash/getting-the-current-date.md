---
title:    "Bash: Obtendo a data atual"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que?

Muitas vezes, em nossos projetos de programação, precisamos saber a data atual. Isso pode ser importante para realizar cálculos de tempo, gerar relatórios ou simplesmente registrar informações na hora certa. Felizmente, em Bash, existe uma maneira simples de obter a data atual do sistema. Vamos explorar como fazer isso!

## Como fazer

Para obter a data atual em Bash, podemos usar o comando `date` seguido de um formato específico para a data. Por exemplo:

```
Bash:
date +%d/%m/%Y

Output:
05/09/2021
```

No exemplo acima, usamos o formato `%d/%m/%Y`, que nos dá a data atual no formato dia/mês/ano. Existem vários outros formatos que podemos utilizar. Aqui estão alguns dos mais comuns:

- `%d`: dia do mês (01 a 31)
- `%m`: mês (01 a 12)
- `%Y`: ano com 4 dígitos (ex: 2021)
- `%y`: ano com 2 dígitos (ex: 21)
- `%H`: hora em formato 24 horas (00 a 23)
- `%M`: minutos (00 a 59)
- `%S`: segundos (00 a 59)

Você pode combinar esses formatos para obter informações mais detalhadas sobre a data e hora atual. Por exemplo, `date +%d/%m/%Y %H:%M` nos daria a data e hora atual no formato dia/mês/ano hora:minutos.

## Deep Dive

Além dos formatos mencionados acima, o comando `date` também possui outras opções interessantes. Podemos, por exemplo, alterar a data retornada, utilizando o parâmetro `-d`, seguido de uma data específica. Por exemplo:

```
Bash:
date -d "next monday" +%d/%m/%Y

Output:
13/09/2021
```

Isso nos daria a data da próxima segunda-feira no formato dia/mês/ano. Também podemos especificar uma data específica utilizando o formato "MMDDhhmmYYYY", onde MM é o mês, DD é o dia, hh é a hora (formato 24 horas) e mm é o minuto.

Outra opção interessante é o uso de variáveis de ambiente. Podemos armazenar a data atual em uma variável e usá-la posteriormente em nosso código. Por exemplo:

```
Bash:
data_atual=$(date + %d/%m/%Y)
echo "A data atual é: $data_atual"

Output:
A data atual é: 05/09/2021
```

## Veja também

Para mais informações sobre o comando `date` e os formatos disponíveis, você pode conferir a documentação oficial do Bash [neste link](https://linux.die.net/man/1/date). Também é possível utilizar a função `man date` no terminal para acessar a documentação diretamente.

Espero que este artigo tenha sido útil para você aprender como obter a data atual em Bash. Agora você pode usá-la em seus projetos de programação de forma simples e eficiente. Se você tiver alguma dúvida ou sugestão, por favor, deixe um comentário abaixo. Obrigado por ler!