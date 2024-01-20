---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Lua: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Calcular uma data futura ou passada é uma tarefa comum em programação. Frequentemente utilizada para criar eventos programados, registrar datas de expiração, entre outros.

## Como fazer:

Aqui está um exemplo simples de como você pode adicionar ou subtrair dias numa data em Lua:

```Lua 
os.execute('´date -d "+2 days"´')
```
Este comando está somando dois dias à data atual. O output será a nova data.

Para subtrair dias, você faria o seguinte:

```Lua 
os.execute('´date -d "-3 days"´')
```
Este comando está subtraindo três dias da data atual. O output será a nova data.

## Aprofundamento

A manipulação de datas é um tópico complexo em qualquer linguagem de programação. A linguagem Lua usa o pacote `os`, que fornece funções simples, mas não muito precisas para manipular datas. Para necessidades mais precisas, voçe deve considerar pacotes mais robustos como o `lua-date` ou o `luatz`. 

A maior parte das linguagens de programação modernas têm bibliotecas ou pacotes dedicados para lidar com datas e horários, já que manipular datas e horários por si só pode introduzir vários bugs e complicações, principalmente relacionados ao horário de verão e fusos horários.

## Veja Também

Para mais detalhes e recursos, por favor veja os links a seguir:

- [Lua-users wiki: Dates and Time](http://lua-users.org/wiki/DatesAndTimes): Uma excelente entrada sobre como trabalhar com datas e horas em Lua.
- [lua-date: Date & Time library for Lua](https://github.com/Tieske/date): Esta biblioteca é um pacote mais robusto para a manipulação de datas e horários em Lua.
- [luatz: library for date and time manipulation](https://github.com/daurnimator/luatz): Outra opção para manipulação de datas e horários em Lua.