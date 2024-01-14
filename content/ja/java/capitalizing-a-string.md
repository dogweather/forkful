---
title:    "Java: 文字列の先頭を大文字に変換する"
keywords: ["Java"]
---

{{< edit_this_page >}}

##なぜ
文字列の大文字化を行うかを説明する1-2文。

##やり方
 "```java
public class Main {
    public static void main(String[] args) {
        String name = "john doe";
        String capitalizedName = name.toUpperCase();
        System.out.println(capitalizedName);
    }
} 
```" 
のようなコードブロックを使用してコーディングの例とサンプルの出力を示します。

##深い潜入
文字列の大文字化についての詳しい情報。小さな文字と大文字を区別する方法や、異なる言語での大文字化方法について説明します。

##参考リンク
- [Java String文書 - 大きな文字](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toUpperCase())
- [ASCIIコード表](https://qiita.com/y-some/items/ec265e4a8f5bd927b69b)
- [Pythonの場合の文字列の大文字化方法](https://www.javatpoint.com/python-string-casing#:~:text=Python%20String%20upper()%2C%20lower()%2C%20capitalize()%20functions,first%20letter%20of%20the%20string.)