---
date: 2024-01-26 04:15:17.386747-07:00
description: "Miten: REPL:n k\xE4ynnist\xE4minen Javassa on yksinkertaista `jshell`-ty\xF6\
  kalun avulla, joka esiteltiin Java 9:ss\xE4. N\xE4in saat k\xE4tesi siihen ja aloitat\u2026"
lastmod: '2024-03-13T22:44:56.446774-06:00'
model: gpt-4-0125-preview
summary: "REPL:n k\xE4ynnist\xE4minen Javassa on yksinkertaista `jshell`-ty\xF6kalun\
  \ avulla, joka esiteltiin Java 9:ss\xE4."
title: "Interaktiivisen komentotulkin (REPL) k\xE4ytt\xF6"
weight: 34
---

## Miten:
REPL:n käynnistäminen Javassa on yksinkertaista `jshell`-työkalun avulla, joka esiteltiin Java 9:ssä. Näin saat kätesi siihen ja aloitat perusistunnon:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  luotiin metodi sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

Poistu milloin tahansa komennolla `/exit`.

```Java
jshell> /exit
|  Näkemiin
```

## Syväsukellus
Ennen `jshell`-työkalua Javan ohjelmoijilla ei ollut virallista REPL:iä, toisin kuin Pythonin tai Rubyn kehittäjillä. He käyttivät IDE:itä tai kirjoittivat kokonaisia ohjelmia jopa triviaaleja tehtäviä varten. `jshell` oli pelinmuuttaja Java 9:stä alkaen, sillä se kavensi tuon kuilun.

Vaihtoehtoja ovat muun muassa online-kääntäjät tai IDE-lisäosat, mutta ne eivät yllä `jshell`in välittömyyteen. Sisäisesti `jshell` käyttää Java Compiler API:ta koodipätkien suorittamiseen, mikä on melko siistiä. Se on enemmän kuin pelikenttä – se voi tuoda kirjastoja, määritellä luokkia ja enemmän. Tämä tekee siitä vankan työkalun prototyyppien tekoon.

## Katso Myös
- [JShell Käyttäjän Opas](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Java Platform, Standard Edition Työkalujen Viite](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java Compiler API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
