---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:22.454499-07:00
description: "\u65B9\u6CD5\uFF1A #."
lastmod: '2024-03-13T22:44:42.081276-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 方法：


### 標準的なKotlinの使用
Kotlinには独自の日付と時間のAPIはなく、この機能のためにJava 標準ライブラリに依存しています。ここでは、現在の日付を取得する方法を示します：

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("今日の日付: $today")
}
```

**サンプル出力：**
```
今日の日付: 2023-04-05
```

### java.util.Dateの使用
日付と時間の両方が必要な操作の場合、`java.util.Date`が好ましいかもしれません。

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("現在の日付と時刻: $currentDate")
}
```

**サンプル出力：**
```
現在の日付と時刻: Wed Apr 05 15:20:45 GMT 2023
```

### Joda-Time ライブラリの使用
Java 8が新しい日付と時間のAPIを導入する前、Joda-TimeはJavaとKotlinで日時操作の事実上の標準でした。多くのプロジェクトではもはや必要ないとはいえ、レガシーな理由や個人的な好みで使用されていることもあります。

プロジェクトのbuild.gradleファイルにJoda-Timeライブラリを追加します：
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("今日の日付: $today")
}
```

**サンプル出力：**
```
今日の日付: 2023-04-05
```

### ThreeTenABPのAndroidでの使用
Android開発においては、Android API Level 26より前のバージョンに対して、Java Time APIのバックポートをThreeTen Android Backport Projectを通じて使用することを推奨します。

アプリのbuild.gradleファイルに依存関係を追加します：
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Applicationクラスで初期化します：
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MyApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

それから、このように使用できます：
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("今日の日付: $today")
}
```

**サンプル出力：**
```
今日の日付: 2023-04-05
```
