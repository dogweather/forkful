---
title:                "正規表現の使用"
aliases:
- /ja/java/using-regular-expressions/
date:                  2024-02-03T19:17:17.932340-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
