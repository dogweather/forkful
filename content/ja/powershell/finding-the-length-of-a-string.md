---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の長さを見つけるとは、文字列に含まれる文字の数を数えることを指します。プログラマーはこの操作を行うことで、入力データの精度を確認し、あるいは必要なメモリの量を推定します。

## 使い方：

PowerShellにおいて、文字列の長さを取得するためには `.Length` プロパティを使用します。

```PowerShell
$string = "Hello, PowerShell"
$length = $string.Length
$length
```

上記のスクリプトを実行すると、以下のような結果が表示されます：

```PowerShell
16
```

## ディープダイブ：

この操作は、具体的な文字数を特定するという基礎的なコンセプトから始まります。その歴史はコンピュータープログラミングの初期にまでさかのぼります。

代替として、一部のプログラミング言語では `strlen()` や `length()` などの関数を使用しますが、PowerShellでは `.Length` プロパティが一般的に使用されます。

実装の詳細については、文字列はそのコンピューターのメモリ内で各文字を個別のメモリアドレスに格納し、`.Length` プロパティはこれらのアドレスを数えるという形を取ります。

## 参照：

- PowerShellについての詳細\(英語\)：[Microsoft Official Documentation](https://docs.microsoft.com/ja-jp/powershell/)
- 文字列の長さの取り扱い\(英語\)：[Stack Overflow](https://stackoverflow.com/questions/4652913/how-to-get-string-length-in-powershell)
- PowerShellスクリプトの基礎\(英語\)：[Microsoft TechNet Blog](https://blogs.technet.microsoft.com/heyscriptingguy/)