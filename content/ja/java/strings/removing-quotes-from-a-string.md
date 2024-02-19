---
aliases:
- /ja/java/removing-quotes-from-a-string/
date: 2024-01-26 03:40:13.201222-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\
  \u304F\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u304B\u3089\u30B7\
  \u30F3\u30B0\u30EB\uFF08' '\uFF09\u3001\u30C0\u30D6\u30EB\uFF08\" \"\uFF09\u3001\
  \u307E\u305F\u306F\u305D\u306E\u4E21\u65B9\u306E\u5F15\u7528\u7B26\u3092\u524A\u9664\
  \u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u3092\u6D88\u6BD2\u3059\u308B\u3001\u30C7\
  \u30FC\u30BF\u3092\u4FDD\u5B58\u306E\u305F\u3081\u306B\u6E96\u5099\u3059\u308B\u3001\
  \u307E\u305F\u306F\u5F15\u7528\u7B26\u304C\u4E0D\u8981\u3067\u6F5C\u5728\u7684\u306B\
  \u554F\u984C\u3068\u306A\u308B\u53EF\u80FD\u6027\u306E\u3042\u308B\u89E3\u6790\u30BF\
  \u30B9\u30AF\u3092\u7C21\u7565\u5316\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.790484
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\
  \u304F\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u304B\u3089\u30B7\
  \u30F3\u30B0\u30EB\uFF08' '\uFF09\u3001\u30C0\u30D6\u30EB\uFF08\" \"\uFF09\u3001\
  \u307E\u305F\u306F\u305D\u306E\u4E21\u65B9\u306E\u5F15\u7528\u7B26\u3092\u524A\u9664\
  \u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u3092\u6D88\u6BD2\u3059\u308B\u3001\u30C7\
  \u30FC\u30BF\u3092\u4FDD\u5B58\u306E\u305F\u3081\u306B\u6E96\u5099\u3059\u308B\u3001\
  \u307E\u305F\u306F\u5F15\u7528\u7B26\u304C\u4E0D\u8981\u3067\u6F5C\u5728\u7684\u306B\
  \u554F\u984C\u3068\u306A\u308B\u53EF\u80FD\u6027\u306E\u3042\u308B\u89E3\u6790\u30BF\
  \u30B9\u30AF\u3092\u7C21\u7565\u5316\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
文字列から引用符を取り除くとは、テキストデータからシングル（' '）、ダブル（" "）、またはその両方の引用符を削除することを意味します。プログラマーは、入力を消毒する、データを保存のために準備する、または引用符が不要で潜在的に問題となる可能性のある解析タスクを簡略化するためにこれを行います。

## 方法
テキストから邪魔な引用符を引き抜きましょう。「replace()」メソッドを使って素早い修正を行い、困難な問題には正規表現を使います。

```java
public class QuoteRemover {
    public static void main(String[] args) {
        String stringWithQuotes = "\"Hello, 'World'!\"";
        String withoutQuotes = stringWithQuotes.replace("\"", "").replace("'", "");
        System.out.println(withoutQuotes); // Hello, World!

        // パターン愛好家のために正規表現を使ってみましょう
        String stringWithMixedQuotes = "\"Java\" and 'Programming'";
        String cleanString = stringWithMixedQuotes.replaceAll("[\"']", "");
        System.out.println(cleanString); // Java and Programming
    }
}
```

## 深堀り
昔々、文字列の中の引用符はそれほど問題ではありませんでした—システムはよりシンプルで、データはそれほど混乱していませんでした。しかし、複雑なデータ形式（JSON、XML）の出現とデータ交換の必要性により、引用符の管理が重要になりました。代替案の話をすると、パーサーを書き、各文字をループして新しい文字列を構築することもできます（雨の日には楽しいかもしれません）。また、文字をエスケープするか、ロケールに応じて異なるタイプの引用符を扱うなど、より洗練されたオプションを提供してくれるサードパーティのライブラリもあります。実装にあたっては、コンテキストなしで引用符を取り除くと、データの意味や構造が変わる可能性があることを念頭に置いてください。「なぜ」を「どのように」するかを考える前に常に考慮してください。

## 参照
- 正規表現についてもっと詳しく知りたい方は、公式のJavaドキュメントをご覧ください：https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- 引用符を取り除く代わりにエスケープする必要がありますか？Stack Overflowがお助けします：https://stackoverflow.com/questions/383551/escape-string-for-sql-insert
- JavaでのJSON処理？おそらく引用符とよく出会うでしょう。ここから始めましょう：https://www.oracle.com/technical-resources/articles/java/json.html
