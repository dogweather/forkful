---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:13.738766-07:00
description: "Como fazer: O Kotlin n\xE3o possui sua pr\xF3pria API de data e hora,\
  \ mas depende da Biblioteca Padr\xE3o Java para essa funcionalidade. Veja como voc\xEA\
  \ pode obter\u2026"
lastmod: '2024-03-13T22:44:46.552694-06:00'
model: gpt-4-0125-preview
summary: "O Kotlin n\xE3o possui sua pr\xF3pria API de data e hora, mas depende da\
  \ Biblioteca Padr\xE3o Java para essa funcionalidade."
title: Obtendo a data atual
weight: 29
---

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
