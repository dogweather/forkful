---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:22.454499-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3044\u3066\u3001\
  \u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u306F\u3001\
  \u958B\u767A\u8005\u304C\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\
  \u73FE\u5728\u306E\u65E5\u4ED8\u306B\u30A2\u30AF\u30BB\u30B9\u3057\u3001\u8868\u793A\
  \u3057\u305F\u308A\u3001\u64CD\u4F5C\u3057\u305F\u308A\u3059\u308B\u3053\u3068\u3092\
  \u53EF\u80FD\u306B\u3059\u308B\u57FA\u672C\u7684\u306A\u4F5C\u696D\u3067\u3059\u3002\
  \u3053\u306E\u6A5F\u80FD\u6027\u306F\u3001\u30ED\u30B0\u8A18\u9332\u3084\u30BF\u30A4\
  \u30E0\u30B9\u30BF\u30F3\u30D4\u30F3\u30B0\u30A4\u30D9\u30F3\u30C8\u304B\u3089\u65E5\
  \u4ED8\u306B\u57FA\u3065\u304F\u8A08\u7B97\u307E\u3067\u3001\u3042\u3089\u3086\u308B\
  \u3053\u3068\u306B\u3068\u3063\u3066\u91CD\u8981\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:42.081276-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3044\u3066\u3001\
  \u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u306F\u3001\
  \u958B\u767A\u8005\u304C\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\
  \u73FE\u5728\u306E\u65E5\u4ED8\u306B\u30A2\u30AF\u30BB\u30B9\u3057\u3001\u8868\u793A\
  \u3057\u305F\u308A\u3001\u64CD\u4F5C\u3057\u305F\u308A\u3059\u308B\u3053\u3068\u3092\
  \u53EF\u80FD\u306B\u3059\u308B\u57FA\u672C\u7684\u306A\u4F5C\u696D\u3067\u3059\u3002\
  \u3053\u306E\u6A5F\u80FD\u6027\u306F\u3001\u30ED\u30B0\u8A18\u9332\u3084\u30BF\u30A4\
  \u30E0\u30B9\u30BF\u30F3\u30D4\u30F3\u30B0\u30A4\u30D9\u30F3\u30C8\u304B\u3089\u65E5\
  \u4ED8\u306B\u57FA\u3065\u304F\u8A08\u7B97\u307E\u3067\u3001\u3042\u3089\u3086\u308B\
  \u3053\u3068\u306B\u3068\u3063\u3066\u91CD\u8981\u3067\u3059\u3002."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 何となぜ？
プログラミングにおいて、現在の日付を取得することは、開発者がアプリケーション内で現在の日付にアクセスし、表示したり、操作したりすることを可能にする基本的な作業です。この機能性は、ログ記録やタイムスタンピングイベントから日付に基づく計算まで、あらゆることにとって重要です。

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
