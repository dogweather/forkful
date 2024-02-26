---
date: 2024-01-20 17:52:43.209660-07:00
description: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3068\u306F\u3001\u30B3\u30FC\u30C9\
  \u306E\u52D5\u4F5C\u3092\u7406\u89E3\u3057\u3084\u3059\u304F\u3059\u308B\u305F\u3081\
  \u306B\u3001\u5909\u6570\u306E\u5024\u3084\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u72B6\
  \u614B\u3092\u8868\u793A\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u554F\u984C\u3092\u898B\u3064\u3051\u3084\u3059\u304F\u3059\
  \u308B\u305F\u3081\u3001\u307E\u305F\u306F\u30B3\u30FC\u30C9\u306E\u52D5\u4F5C\u304C\
  \u610F\u56F3\u3057\u305F\u901A\u308A\u304B\u3092\u78BA\u8A8D\u3059\u308B\u305F\u3081\
  \u306B\u3088\u304F\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:39.982448-07:00'
model: gpt-4-1106-preview
summary: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3068\u306F\u3001\u30B3\u30FC\u30C9\
  \u306E\u52D5\u4F5C\u3092\u7406\u89E3\u3057\u3084\u3059\u304F\u3059\u308B\u305F\u3081\
  \u306B\u3001\u5909\u6570\u306E\u5024\u3084\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u72B6\
  \u614B\u3092\u8868\u793A\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u554F\u984C\u3092\u898B\u3064\u3051\u3084\u3059\u304F\u3059\
  \u308B\u305F\u3081\u3001\u307E\u305F\u306F\u30B3\u30FC\u30C9\u306E\u52D5\u4F5C\u304C\
  \u610F\u56F3\u3057\u305F\u901A\u308A\u304B\u3092\u78BA\u8A8D\u3059\u308B\u305F\u3081\
  \u306B\u3088\u304F\u4F7F\u3044\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

デバッグ出力とは、コードの動作を理解しやすくするために、変数の値やプログラムの状態を表示することです。プログラマーは問題を見つけやすくするため、またはコードの動作が意図した通りかを確認するためによく使います。

## How to: (方法)

Javaでデバッグ出力をする基本的な方法を紹介します。

```java
public class DebugExample {
    public static void main(String[] args) {
        int sum = 0;
        for (int i = 0; i < 10; i++) {
            sum += i;
            System.out.println("i = " + i + ", sum = " + sum); // デバッグ出力
        }
    }
}
```

実行結果:

```
i = 0, sum = 0
i = 1, sum = 1
i = 2, sum = 3
i = 3, sum = 6
i = 4, sum = 10
i = 5, sum = 15
i = 6, sum = 21
i = 7, sum = 28
i = 8, sum = 36
i = 9, sum = 45
```

## Deep Dive (深掘り)

デバッグ出力の歴史は古く、昔のプログラミングでは印刷機やポンチカードに直接出力することから始まりました。今では、`System.out.println()` のようなコマンドでコンソールに出力するのが一般的です。

代替として、より強力なロギングフレームワーク（log4j、SLF4Jなど）があります。これらは出力のレベル（INFO、DEBUG、ERRORなど）を設定したり、コンソール以外の場所（ファイル、ネットワーク）に出力したりできます。

デバッグ出力の実装には、条件付きで出力を切り替える（本番環境でのパフォーマンスへの影響を避けるため）などの考慮が必要です。Javaではコンパイル時に指定する `assert` ステートメントも使え、条件がfalseの時にメッセージを出力します。

## See Also (関連情報)

- [Oracle Java Tutorials - The `print` and `println` Methods](https://docs.oracle.com/javase/tutorial/essential/io/formatting.html)
- [Apache log4j 2](https://logging.apache.org/log4j/2.x/)
- [Simple Logging Facade for Java (SLF4J)](http://www.slf4j.org/)
- [Java Assertions](https://docs.oracle.com/javase/8/docs/technotes/guides/language/assert.html)
