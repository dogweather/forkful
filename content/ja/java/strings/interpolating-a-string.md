---
date: 2024-01-20 17:51:19.504040-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.925931-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
