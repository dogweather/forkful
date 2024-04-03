---
date: 2024-01-20 18:03:57.787759-07:00
description: 'Como Fazer: Vamos criar um projeto Java simples. Usarei o JDK 18, o
  mais recente no momento do conhecimento.'
lastmod: '2024-03-13T22:44:46.456710-06:00'
model: gpt-4-1106-preview
summary: Vamos criar um projeto Java simples.
title: Iniciando um novo projeto
weight: 1
---

## Como Fazer:
Vamos criar um projeto Java simples. Usarei o JDK 18, o mais recente no momento do conhecimento.

```Java
public class MeuPrimeiroProjeto {
    public static void main(String[] args) {
        System.out.println("Olá, mundo Java!");
    }
}
```

Quando você executa esse código, o resultado é:

```
Olá, mundo Java!
```

## Mergulho Profundo
Iniciar um projeto Java no passado significava configurar manualmente sua estrutura de diretórios e arquivos. Hoje, temos ferramentas como Maven e Gradle que fazem isso por nós. Maven, por exemplo, é um gerenciador de projetos que segue a convenção sobre configuração, padrão que ajuda a manter a consistência entre projetos Java.

```xml
<project>
    <!-- configurações do projeto -->
</project>
```

Gradle, outra ferramenta poderosa, usa Groovy ou Kotlin para script e é conhecido por sua flexibilidade e performance.

```groovy
apply plugin: 'java'
```

Em termos de IDEs, você tem escolhas como Eclipse, IntelliJ IDEA e NetBeans, que podem gerar e gerir o projeto para você.

A implementação de um projeto depende do seu objetivo. Projetos Java podem ser aplicações de desktop, web, servidores, jogos, entre outros. E com o advento de frameworks como Spring Boot, criar e configurar novos projetos se tornou mais simples e rápido. Spring Boot, por exemplo, usa uma abordagem opinativa para configuração que reduz significativamente o boilerplate e o tempo de configuração.

## Veja Também
- [Introdução ao Maven](https://maven.apache.org/guides/getting-started/)
- [Documentação do Gradle](https://docs.gradle.org/current/userguide/userguide.html)
- [Spring Boot](https://spring.io/projects/spring-boot)
- [Tutorial de Java no Oracle](https://docs.oracle.com/javase/tutorial/)
