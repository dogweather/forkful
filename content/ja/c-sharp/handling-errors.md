---
title:                "エラー処理"
date:                  2024-01-26T00:52:59.048461-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/handling-errors.md"
---

{{< edit_this_page >}}

## 何となぜ?

C#でのエラー処理は、予期しない問題を管理することについてです—例えば、自分の靴紐につまずくようなこと。プログラムは不正なデータや不安定な接続でつまずくことがあります。エラーを処理することで、ソフトウェアが顔面から転ぶのを防ぎ、優雅に回復させます。

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
