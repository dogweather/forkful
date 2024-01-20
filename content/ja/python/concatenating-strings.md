---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ?
文字列の連結は、複数の文字列を一つにつなげる処理のことです。これは、メッセージの生成、データの処理、出力のフォーマットに必需です。

## どのように:
Pythonで文字列を連結する最も一般的な方法は、`+` 演算子を使用することです。

```Python
string1 = 'こんにちは、'
string2 = '世界!'
print(string1 + string2)
```

出力:

```Python
'こんにちは、世界!'
```

`join()`メソッドを使用する別の方法もあります。これは、リストの全ての文字列要素を連結します。

```Python
strings = ['こんにちは、', '世界!']
print(''.join(strings))
```

出力:

```Python
'こんにちは、世界!'
```

## ディープダイブ :
文字列連結は非常に古いコンセプトで、初期のプログラミング言語から存在しています。Pythonでの`+`演算子による連結は、最も直感的であるためよく用いられます。しかし、大量の文字列を連結する場合は、`join()`メソッドが効率的です。

また、`%`演算子や`format()`関数、さらに新しい`f-string`という書き方も存在します。これらはリテラルの中に文字列を挿入する場合に便利で、読みやすいコードを書くことが可能です。

```Python
name = '世界'
print(f'こんにちは、{name}!')
```

出力:

```Python
'こんにちは、世界!'
```

## 参考に :
1. [Python公式ドキュメンテーション: 文字列メソッド](https://docs.python.org/ja/3/library/stdtypes.html#string-methods)
2. [PEP 498 -- Literal String Interpolation](https://peps.python.org/pep-0498/) (f-stringのこと)

以上がPythonでの文字列連結についての紹介です。具体的な使い方や選択肢を説明するため、さまざまな情報を提供しました。これらを参考に、あなたのプログラムに最適な解決策を見つけてください。