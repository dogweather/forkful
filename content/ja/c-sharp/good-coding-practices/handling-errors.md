---
date: 2024-01-26 00:52:59.048461-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066: try-catch\u30D6\u30ED\u30C3\
  \u30AF\u304B\u3089\u59CB\u3081\u307E\u3057\u3087\u3046\u3002\u3053\u308C\u306F\u3001\
  \u7DB1\u6E21\u308A\u82B8\u4EBA\u306E\u4E0B\u306B\u5B89\u5168\u7DB2\u3092\u5F35\u308B\
  \u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\u5F7C\u3089\u304C\u6ED1\u3063\u305F\
  \u5834\u5408\u306B\u3001\u589C\u843D\u3059\u308B\u3053\u3068\u306A\u304F\u6355\u307E\
  \u3048\u3089\u308C\u308B\u306E\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:42.134323-06:00'
model: gpt-4-1106-preview
summary: "try-catch\u30D6\u30ED\u30C3\u30AF\u304B\u3089\u59CB\u3081\u307E\u3057\u3087\
  \u3046\u3002\u3053\u308C\u306F\u3001\u7DB1\u6E21\u308A\u82B8\u4EBA\u306E\u4E0B\u306B\
  \u5B89\u5168\u7DB2\u3092\u5F35\u308B\u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\
  \u5F7C\u3089\u304C\u6ED1\u3063\u305F\u5834\u5408\u306B\u3001\u589C\u843D\u3059\u308B\
  \u3053\u3068\u306A\u304F\u6355\u307E\u3048\u3089\u308C\u308B\u306E\u3067\u3059."
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## どのようにして:
try-catchブロックから始めましょう。これは、綱渡り芸人の下に安全網を張るようなものです。彼らが滑った場合に、墜落することなく捕まえられるのです。

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] numbers = {1, 2, 3};
            Console.WriteLine(numbers[5]);  // おっと、インデックスが範囲外です！
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("エラーを捕捉: " + e.Message);
        }
    }
}
```

物事がうまくいかない時のサンプル出力：
```
エラーを捕捉: インデックスは配列の範囲外でした。
```

次に、finallyブロックを追加します—これは何があっても実行されるもので、例えば税金を支払うことに似ています。

```C#
try {
    // ここに問題を起こす可能性のあるコード
} catch (SomeSpecificException e) {
    // ここでその特定のエラーを処理
} finally {
    // 上記で何が起ころうとこのコードは実行される
    Console.WriteLine("これは必ず実行されます。");
}
```

## 深掘り
エラー処理はC#が誕生して以来取り入れられています。時間とともに進化してきました。昔のプログラマは、問題を知らせるために戻り値やグローバルフラグに頼っていました—使い勝手が悪く、エラーが発生しやすい方法です。

C#は例外という、より現代的なアプローチを使用します。予期しない事態が発生したときに例外が投げられます。これは、フットボールでプレー中にフラグを投げるようなものです。try、catch、finallyブロックで構成される構造化例外処理は、古いスタイルのエラーチェックに比べて、これらの瞬間をクリアかつクリーンに管理することを可能にしています。

代替手段はありますか？もちろんです。例外がすり抜けた場合の`UnhandledExceptionEventHandler`や、非同期コードでは例外が含まれる`Task`オブジェクトを使ったエラー処理があります。

実装の詳細—細かい文字通りに言えば—は重要です。例外はコストがかかる可能性があり、安易に投げられるとパフォーマンスが低下する可能性があります。そのため、我々は毎日のロジック制御ではなく、例外的なケースに対してそれを使用します。

## 関連項目
- [C# における例外の公式ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [C# 例外処理のベストプラクティス](https://docs.microsoft.com/ja-jp/dotnet/standard/exceptions/best-practices-for-exceptions)
