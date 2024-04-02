---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:17.932340-07:00
description: "Java\u3067\u306E\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u3092\u4F7F\
  \u3046\u3068\u3001\u30B3\u30FC\u30C9\u4E2D\u306E\u6587\u5B57\u5217\u3092\u691C\u7D22\
  \u3001\u64CD\u4F5C\u3001\u307E\u305F\u306F\u691C\u8A3C\u3059\u308B\u305F\u3081\u306E\
  \u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u3092\u5B9A\u7FA9\u3067\u304D\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30ED\u30B0\u30D5\u30A1\u30A4\
  \u30EB\u306E\u89E3\u6790\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u691C\u8A3C\
  \u3001\u30C6\u30AD\u30B9\u30C8\u5185\u306E\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\
  \u306E\u691C\u7D22\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306B\u3053\u308C\u3089\u3092\
  \u4F7F\u7528\u3057\u3001\u6700\u5C0F\u9650\u306E\u52B4\u529B\u3067\u9AD8\u5EA6\u306A\
  \u6587\u5B57\u5217\u51E6\u7406\u3092\u5B9F\u73FE\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.931989-06:00'
model: gpt-4-0125-preview
summary: "Java\u3067\u306E\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u3092\u4F7F\u3046\
  \u3068\u3001\u30B3\u30FC\u30C9\u4E2D\u306E\u6587\u5B57\u5217\u3092\u691C\u7D22\u3001\
  \u64CD\u4F5C\u3001\u307E\u305F\u306F\u691C\u8A3C\u3059\u308B\u305F\u3081\u306E\u7279\
  \u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u3092\u5B9A\u7FA9\u3067\u304D\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30ED\u30B0\u30D5\u30A1\u30A4\u30EB\
  \u306E\u89E3\u6790\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u691C\u8A3C\u3001\
  \u30C6\u30AD\u30B9\u30C8\u5185\u306E\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u306E\
  \u691C\u7D22\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306B\u3053\u308C\u3089\u3092\u4F7F\
  \u7528\u3057\u3001\u6700\u5C0F\u9650\u306E\u52B4\u529B\u3067\u9AD8\u5EA6\u306A\u6587\
  \u5B57\u5217\u51E6\u7406\u3092\u5B9F\u73FE\u3057\u307E\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 何となぜ？

Javaでの正規表現（regex）を使うと、コード中の文字列を検索、操作、または検証するための特定のパターンを定義できます。プログラマーは、ログファイルの解析、ユーザー入力の検証、テキスト内の特定のパターンの検索などのタスクにこれらを使用し、最小限の労力で高度な文字列処理を実現します。

## 使い方：

Javaでのregexの組み込みサポートは、主に`java.util.regex`パッケージの`Pattern`クラスと`Matcher`クラスを通じて提供されます。ここに、文字列内の単語のすべての出現を大文字・小文字を区別せずに見つけて出力する簡単な例を示します。

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex is great for parsing. Parsing with regex is powerful.";
        String wordToFind = "parsing";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Found '" + matcher.group() + "' at position " + matcher.start());
        }
    }
}
```

出力：
```
Found 'parsing' at position 16
Found 'Parsing' at position 31
```

文字列を分割するようなタスクの場合、regexを使用して`String`クラスの`split()`メソッドを使用できます。

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

出力：
```
Java
Python
Ruby
JavaScript
```

Javaでregexを扱う際、複雑なタスクを簡素化する外部ライブラリが役立つ場合があります。Javaでregexを扱うための人気のサードパーティライブラリの一つに`Apache Commons Lang`があります。これは、一部のregexタスクをより簡単にするような`StringUtils`などのユーティリティを提供します。ここに、サブストリングの一致回数を数えるための使用方法を示します。

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' appears " + count + " times.");
    }
}
```

Apache Commons Langを使用するには、プロジェクトに含める必要があります。Mavenを使用している場合、この依存関係を`pom.xml`に追加してください。

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- 最新バージョンを確認してください -->
</dependency>
```

出力：
```
'processing' appears 2 times.
```
