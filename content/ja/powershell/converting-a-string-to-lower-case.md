---
title:                "「文字列を小文字に変換する」"
html_title:           "PowerShell: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何&なぜ？
文字列を小文字に変換するとは何かを説明する２〜３文と、なぜプログラマーがそれを行うのかを説明します。

文字列を小文字に変換するとは、文字列をすべて小文字に変換することを指します。プログラマーがこれを行う理由の1つは、検索や比較を行う際に大文字と小文字を区別しないようにするためです。また、コンソールの出力を統一的にするためにも使用されます。

## 使い方：
以下のコードブロックに記載されたコーディングの例を参考に、文字列を小文字に変換する方法を学んでください。

```PowerShell
# テキストを小文字に変換する方法
$text = "hello WORLD"
$text.ToLower()
```

```
# 出力結果
hello world
```

上記のコードでは、変数 `$text` に格納されている文字列を `ToLower()` メソッドを使って小文字に変換しています。

## 深堀り：
以下の項目について、文字列を小文字に変換することに関するさらなる情報をお伝えします。

1. 歴史的背景
文字列を小文字に変換する機能は、検索エンジンの登場に伴いより重要度を増してきました。検索エンジンは大文字と小文字を区別せず、小文字に統一されたキーワードやコンテンツを扱うため、プログラム言語にもこの機能が導入されました。

2. 代替手段
文字列を小文字に変換するための別の方法として、`-cmatch` というオプションを使用する方法があります。これは、大文字と小文字を区別しないように比較することを指します。

3. 実装の詳細
PowerShellでは、`ToLower()` メソッドを使用して文字列を小文字に変換することができます。このメソッドは、文字列オブジェクトのメソッドとして利用できます。

## 関連情報：
以下のリンクから、文字列を小文字に変換する方法に関するより詳細な情報を学ぶことができます。

- [Microsoft公式ドキュメント：`ToLower()` メソッド](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- [PowerShellでの文字列操作方法についてのチュートリアル](https://www.udemy.com/course/powershell-string-operations/)