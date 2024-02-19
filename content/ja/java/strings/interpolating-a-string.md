---
aliases:
- /ja/java/interpolating-a-string/
date: 2024-01-20 17:51:19.504040-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u5B9A\u7FA9\u6E08\u307F\
  \u306E\u5909\u6570\u3092\u6587\u5B57\u5217\u306E\u4E2D\u306B\u57CB\u3081\u8FBC\u3080\
  \u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u52D5\u7684\u306A\
  \u30B3\u30F3\u30C6\u30F3\u30C4\u751F\u6210\u3084\u30C7\u30FC\u30BF\u306E\u6574\u5F62\
  \u304C\u5BB9\u6613\u306B\u306A\u308B\u305F\u3081\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3088\u304F\u4F7F\u7528\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.788797
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u5B9A\u7FA9\u6E08\u307F\
  \u306E\u5909\u6570\u3092\u6587\u5B57\u5217\u306E\u4E2D\u306B\u57CB\u3081\u8FBC\u3080\
  \u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u52D5\u7684\u306A\
  \u30B3\u30F3\u30C6\u30F3\u30C4\u751F\u6210\u3084\u30C7\u30FC\u30BF\u306E\u6574\u5F62\
  \u304C\u5BB9\u6613\u306B\u306A\u308B\u305F\u3081\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3088\u304F\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
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
