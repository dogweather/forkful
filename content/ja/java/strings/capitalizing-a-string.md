---
title:                "文字列を大文字にする"
aliases:
- /ja/java/capitalizing-a-string/
date:                  2024-02-03T19:05:55.829288-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
文字列の最初の文字を大文字に変更し、残りを小文字に保つことで文字列を大文字にすることを指します。この一般的な文字列操作タスクは、慣習や文法的な正確さに従ってユーザー名やタイトルを表示用に準備するなど、アプリケーションでテキストをフォーマットするのに役立ちます。

## 方法
Javaの標準ライブラリは、一度に文字列全体を大文字にする直接的な方法を提供していませんが、組み込みのメソッドを組み合わせることでこれを実現できます。もっと洗練されたニーズのために、Apache Commons Langのようなサードパーティーライブラリは、直接的な解決策を提供します。

### Javaの組み込みメソッドを使用する
外部ライブラリなしで文字列を大文字化するには、文字列を単語に分割し、各単語の最初の文字を大文字にしてから再結合します。ここに簡単なアプローチがあります：

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // 出力: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

このコードスニペットは、文字列全体を小文字に変換し、各文字を繰り返し処理して、各単語の最初の文字を大文字に変換します。空白、ピリオド、アポストロフィを単語のセパレーターとして考慮しています。

### Apache Commons Langを使用する

Apache Commons Langライブラリは、さまざまなエッジケースやデリミターを処理する`WordUtils.capitalizeFully()`メソッドで、よりエレガントな解決策を提供します：

```java
// 依存関係を追加: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // 出力: "Hello, World!"
    }
}
```

このメソッドを使用するには、Apache Commons Langライブラリをプロジェクトに追加する必要があります。このライブラリの方法は、各単語の最初の文字を大文字化するだけでなく、各単語の残りの文字を小文字に変換し、文字列全体で一貫した大文字化パターンを保証します。
