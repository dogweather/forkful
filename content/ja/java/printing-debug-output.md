---
title:                "デバッグ出力を表示する"
date:                  2024-01-20T17:52:43.209660-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/printing-debug-output.md"
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
