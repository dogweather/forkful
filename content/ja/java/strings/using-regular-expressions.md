---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:17.932340-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Java\u3067\u306Eregex\u306E\u7D44\u307F\u8FBC\
  \u307F\u30B5\u30DD\u30FC\u30C8\u306F\u3001\u4E3B\u306B`java.util.regex`\u30D1\u30C3\
  \u30B1\u30FC\u30B8\u306E`Pattern`\u30AF\u30E9\u30B9\u3068`Matcher`\u30AF\u30E9\u30B9\
  \u3092\u901A\u3058\u3066\u63D0\u4F9B\u3055\u308C\u307E\u3059\u3002\u3053\u3053\u306B\
  \u3001\u6587\u5B57\u5217\u5185\u306E\u5358\u8A9E\u306E\u3059\u3079\u3066\u306E\u51FA\
  \u73FE\u3092\u5927\u6587\u5B57\u30FB\u5C0F\u6587\u5B57\u3092\u533A\u5225\u305B\u305A\
  \u306B\u898B\u3064\u3051\u3066\u51FA\u529B\u3059\u308B\u7C21\u5358\u306A\u4F8B\u3092\
  \u793A\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.830084-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

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
