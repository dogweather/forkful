---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列補間は、文中の特定の部分を変数の値で置き換えるプログラミングテクニックです。これにより、動的にコード内の文字列を変更することが可能になり、コードはより柔軟で読みやすくなります。

## どのようにして？
Pythonで文字列補間を行う方法はいくつかあります。ここでは2つの主要な方法を示します。

### 方法1: f-string

Python 3.6以降、f-stringと呼ばれる新しい機能が追加されました。これは非常に直感的で効率的な方法です。

```python
name = "John"
print(f'Hello, {name}!')

# 出力: Hello, John!
```

### 方法2: str.format()

古いバージョンのPythonを使用している場合は、str.format（）メソッドを使用できます。これは、より古いバージョンでもサポートされています。

```python
name = "John"
print('Hello, {}!'.format(name))

# 出力: Hello, John!
```

## 深いダイブ
文字列補間は古くから存在し、他の多くのプログラミング言語にも見られます。Pythonでは、以前は％フォーマット演算子を使用する方法が主流でしたが、より新しいバージョンではf-stringまたはstr.formatを使用することが推奨されています。それぞれの手法には各々の利点や短所がありますが、コードの読みやすさと効率性から見れば、f-stringが最も優れた選択となるでしょう。

## 参照
* [公式Python文字列補間ドキュメンテーション](https://docs.python.org/3/library/string.html#formatstrings)
* [文字列書式設定のヒストリー](https://docs.python.org/3/tutorial/inputoutput.html#old-string-formatting)