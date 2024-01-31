---
title:                "文字列の補間"
date:                  2024-01-20T17:51:19.504040-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"

category:             "Java"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間とは、定義済みの変数を文字列の中に埋め込むことです。これにより、動的なコンテンツ生成やデータの整形が容易になるため、プログラマーはよく使用します。

## How to: (方法)
```java
public class StringInterpolationExample {
    public static void main(String[] args) {
        String name = "山田";
        int age = 30;
        
        // Java 15以降では、テキストブロックの利用が可能
        String greeting = """
                こんにちは、%sさん。
                あなたの年齢は%d歳ですね。
                """.formatted(name, age);
        
        System.out.println(greeting);
    }
}
```
サンプル出力：
```
こんにちは、山田さん。
あなたの年齢は30歳ですね。
```

## Deep Dive (深掘り)
歴史的にJavaでは文字列補間は直接的ではありませんでした。`String.format()`や`+`演算子を使った方法が主でした。しかし、Java 15からはテキストブロックと`formatted`メソッドによって、他の言語のような直感的な文字列補間が可能になりました。

代替としてStringBuilderや+演算子を使った結合はありますが、パフォーマンスや可読性の点から推奨されません。補間の実装は内部的には`String.format()`を使用するか、パターンマッチングといったより高度な手法を使うことがあります。

## See Also (関連リンク)
- [Oracle Java Documentation](https://docs.oracle.com/en/java/javase/)
- [JEP 355: Text Blocks (Final)](https://openjdk.java.net/jeps/355)
- [String Formatting in Java](https://docs.oracle.com/javase/tutorial/java/data/numberformat.html)
