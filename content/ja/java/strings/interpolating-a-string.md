---
date: 2024-01-20 17:51:19.504040-07:00
description: "How to: (\u65B9\u6CD5) \u6B74\u53F2\u7684\u306BJava\u3067\u306F\u6587\
  \u5B57\u5217\u88DC\u9593\u306F\u76F4\u63A5\u7684\u3067\u306F\u3042\u308A\u307E\u305B\
  \u3093\u3067\u3057\u305F\u3002`String.format()`\u3084`+`\u6F14\u7B97\u5B50\u3092\
  \u4F7F\u3063\u305F\u65B9\u6CD5\u304C\u4E3B\u3067\u3057\u305F\u3002\u3057\u304B\u3057\
  \u3001Java 15\u304B\u3089\u306F\u30C6\u30AD\u30B9\u30C8\u30D6\u30ED\u30C3\u30AF\u3068\
  `formatted`\u30E1\u30BD\u30C3\u30C9\u306B\u3088\u3063\u3066\u3001\u4ED6\u306E\u8A00\
  \u8A9E\u306E\u3088\u3046\u306A\u76F4\u611F\u7684\u306A\u6587\u5B57\u5217\u88DC\u9593\
  \u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3057\u305F\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:55.877557-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6B74\u53F2\u7684\u306BJava\u3067\u306F\u6587\u5B57\u5217\
  \u88DC\u9593\u306F\u76F4\u63A5\u7684\u3067\u306F\u3042\u308A\u307E\u305B\u3093\u3067\
  \u3057\u305F\u3002`String.format()`\u3084`+`\u6F14\u7B97\u5B50\u3092\u4F7F\u3063\
  \u305F\u65B9\u6CD5\u304C\u4E3B\u3067\u3057\u305F\u3002\u3057\u304B\u3057\u3001Java\
  \ 15\u304B\u3089\u306F\u30C6\u30AD\u30B9\u30C8\u30D6\u30ED\u30C3\u30AF\u3068`formatted`\u30E1\
  \u30BD\u30C3\u30C9\u306B\u3088\u3063\u3066\u3001\u4ED6\u306E\u8A00\u8A9E\u306E\u3088\
  \u3046\u306A\u76F4\u611F\u7684\u306A\u6587\u5B57\u5217\u88DC\u9593\u304C\u53EF\u80FD\
  \u306B\u306A\u308A\u307E\u3057\u305F\u3002"
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
