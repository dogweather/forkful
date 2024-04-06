---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:22.454499-07:00
description: "\u65B9\u6CD5\uFF1A Kotlin\u306B\u306F\u72EC\u81EA\u306E\u65E5\u4ED8\u3068\
  \u6642\u9593\u306EAPI\u306F\u306A\u304F\u3001\u3053\u306E\u6A5F\u80FD\u306E\u305F\
  \u3081\u306BJava \u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u4F9D\u5B58\u3057\
  \u3066\u3044\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u73FE\u5728\u306E\u65E5\
  \u4ED8\u3092\u53D6\u5F97\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\uFF1A\
  ."
lastmod: '2024-04-05T22:38:41.624240-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Kotlin\u306B\u306F\u72EC\u81EA\u306E\u65E5\u4ED8\u3068\
  \u6642\u9593\u306EAPI\u306F\u306A\u304F\u3001\u3053\u306E\u6A5F\u80FD\u306E\u305F\
  \u3081\u306BJava \u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u4F9D\u5B58\u3057\
  \u3066\u3044\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u73FE\u5728\u306E\u65E5\
  \u4ED8\u3092\u53D6\u5F97\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\uFF1A\
  ."
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
