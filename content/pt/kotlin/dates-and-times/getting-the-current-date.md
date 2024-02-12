---
title:                "Obtendo a data atual"
aliases: - /pt/kotlin/getting-the-current-date.md
date:                  2024-02-03T19:10:13.738766-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtendo a data atual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Na programação, obter a data atual é uma tarefa fundamental que permite aos desenvolvedores acessar, exibir ou manipular a data atual dentro de suas aplicações. Essa capacidade é crucial para tudo, desde registrar e marcar eventos com data e hora até cálculos baseados em datas.

## Como fazer:

### Usando Kotlin Padrão
O Kotlin não possui sua própria API de data e hora, mas depende da Biblioteca Padrão Java para essa funcionalidade. Veja como você pode obter a data atual:

```kotlin
import java.time.LocalDate

fun main() {
    val hoje = LocalDate.now()
    println("Data de Hoje: $hoje")
}
```

**Saída de exemplo:**
```
Data de Hoje: 2023-04-05
```

### Usando java.util.Date
Para operações que requerem tanto a data quanto a hora, você pode preferir `java.util.Date`.

```kotlin
import java.util.Date

fun main() {
    val dataAtual = Date()
    println("Data e Hora Atuais: $dataAtual")
}
```

**Saída de exemplo:**
```
Data e Hora Atuais: Qua Abr 05 15:20:45 GMT 2023
```

### Usando a Biblioteca Joda-Time
Antes do Java 8 introduzir uma nova API de Data e Hora, Joda-Time era o padrão de facto para operações de data e hora em Java e Kotlin. Mesmo que não seja mais necessário para muitos projetos, alguns ainda podem usá-lo por razões legadas ou preferência pessoal.

Adicione a biblioteca Joda-Time ao arquivo build.gradle do seu projeto:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val hoje = LocalDate.now()
    println("Data de Hoje: $hoje")
}
```

**Saída de exemplo:**
```
Data de Hoje: 2023-04-05
```

### Usando ThreeTenABP para Android
Para o desenvolvimento Android, é recomendado usar o backport da API Time do Java através do Projeto de Backport Android ThreeTen para versões antes do Nível de API Android 26.

Adicione a dependência ao arquivo build.gradle do seu aplicativo:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Inicialize-o na sua classe Application:
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MeuApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

Então, você pode usá-lo assim:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val hoje = LocalDate.now()
    println("Data de Hoje: $hoje")
}
```

**Saída de exemplo:**
```
Data de Hoje: 2023-04-05
```
