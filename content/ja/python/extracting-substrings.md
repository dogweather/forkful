---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？

部分文字列の抽出はある文字列から特定の範囲の文字を裁切り取り出すことです。この技術は、データの分析や検証に大いに役立つため、プログラマーがよく使用します。

## 方法:

以下にPythonでの部分文字列の抽出の例を示します:

```Python
str = "Pythonプログラミング"
print(str[0:6])
```

実行結果:

```Python
Python
```
０から６番目までの位置の文字列が抽出されました。Pythonのインデックスは0から始まるため、位置6は実際には7文字目になります。

## 更なる探求

部分文字列の抽出は古くからあり、Pythonでも使われ続けています。代替手段としては正規表現が有りますが、単純な部分文字列の抽出では普通にスライスが使われます。

Pythonのスライス工法は内部的には高速かつ効率的に動作します。メモリ上の特定範囲を直接参照するため、余分なメモリや処理時間を消費しません。

## 参照

- Python公式ドキュメンテーション: 文字列
  (https://docs.python.org/ja/3/library/stdtypes.html#text-sequence-type-str)

- Python公式チュートリアル: 文字列の操作
  (https://docs.python.org/ja/3/tutorial/introduction.html#strings)