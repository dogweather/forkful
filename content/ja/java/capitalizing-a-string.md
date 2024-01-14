---
title:                "Java: 文字列の先頭を大文字にする"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列の大文字化を行う理由を、たった1-2文で説明します。

文字列の大文字化は、プログラミングにおいてとても重要な操作です。例えば、ユーザーからの入力を正しく扱うために、コード内の文字列を統一された形式に変換する必要があります。また、文字列を比較する際にも大文字と小文字を区別する場合があり、そのような場合にも大文字化は必要です。

## 方法
大文字化を行うための具体的なコーディング例と、その出力結果を「```Java ... ```」のコードブロックを使用して説明します。

#### コード例1
```
String str = "hello world";
String upperCaseStr = str.toUpperCase();
System.out.println(upperCaseStr);
```

#### 出力結果1
```
HELLO WORLD
```

#### コード例2
```
String name = "john";
String capitalizedName = name.substring(0,1).toUpperCase() + name.substring(1);
System.out.println(capitalizedName);
```

#### 出力結果2
```
John
```

## ディープダイブ
文字列の大文字化には、さまざまな方法があります。上記のコード例のように、`toUpperCase()`メソッドを使用する方法以外にも、正規表現やループ処理を使用することで大文字化を行うことができます。また、Java以外のプログラミング言語でも同様の操作が可能です。

## その他の参考資料
- [Java公式ドキュメント - StringクラスのtoUpperCase()メソッド](https://docs.oracle.com/javase/jp/11/docs/api/java.base/java/lang/String.html#toUpperCase())
- [Java公式ドキュメント - Stringクラスのsubstring()メソッド](https://docs.oracle.com/javase/jp/11/docs/api/java.base/java/lang/String.html#substring(int%2Cint))
- [Programming with Mosh - Java Tutorial for Beginners](https://www.youtube.com/watch?v=eIrMbAQSU34)（英語）