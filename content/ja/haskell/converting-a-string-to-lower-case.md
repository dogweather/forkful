---
title:                "文字列を小文字に変換する"
html_title:           "Haskell: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 何してるの？ 
文字列を小文字に変換することは、プログラマーが文字列を操作する際に使用する一般的な方法です。大文字と小文字が区別される場合、小文字に統一することでプログラミングの作業を容易にすることができます。

# 方法: 
Haskellでは、Data.Charモジュールに用意されているtoLower関数を使用して、文字列を小文字に変換することができます。以下のコードは、文字列"Hello World"を小文字に変換し、"hello world"という出力を生成する例です。 

```Haskell
import Data.Char

lowercase :: String -> String
lowercase str = map toLower str

main = do
  let greeting = "Hello World"
  let lowercase_greeting = lowercase greeting
  putStrLn $ lowercase_greeting
```

出力:
```
hello world
```

# 深く掘り下げる: 
文字列を小文字に変換する方法は、プログラミング言語やコンテキストによって異なります。Haskellでは、文字列をリストとして扱うことができるため、toLower関数を使用することで文字列を一文字ずつ変換することができます。また、toLower以外にも、toUpper関数を使用することで文字列を大文字に変換することができます。

他のプログラミング言語では、文字列を直接操作する方法が提供されている場合があります。たとえば、Pythonではstr.lower()メソッドを使用することで、文字列を小文字に変換することができます。

# 関連情報: 
- Data.Charモジュールのドキュメント: https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html
- Pythonの文字列メソッド: https://www.techbeamers.com/python-string-functions/