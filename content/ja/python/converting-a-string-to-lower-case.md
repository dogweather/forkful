---
title:    "Python: 「文字列を小文字に変換する」"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換する必要性は、データの整理や検索のために必要です。

## 方法

```Python
# 文字列の定義
string = "HELLO WORLD"

# 小文字に変換
lowercase_string = string.lower()

# 結果の出力
print(lowercase_string)

# 出力結果: hello world
```

## 深堀り

文字列を小文字に変換する方法は、Pythonの標準ライブラリである`lower()`メソッドを使用することで可能です。このメソッドは文字列の先頭から1文字ずつ取り出し、英字であれば小文字に変換し、それ以外の文字はそのまま返します。

また、大文字と小文字を区別せずに比較したい場合は、`lower()`メソッドを使用して文字列をすべて小文字に変換した上で比較することができます。

## See Also 
[Pythonの文字列に関する公式ドキュメント (英語)](https://docs.python.org/ja/3/library/stdtypes.html#str.lower)  
[文字列の比較についての解説 (日本語)](https://www.python.jp/train/string/string_index.html#id9)