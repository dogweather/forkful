---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
simple_title:         "文字列の先頭を大文字にする"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)

文字列を大文字にするってどういうこと？ それは全ての文字を大文字にするか、あるいは単語のはじめの文字だけを大文字にすることだよ。なんでそんなことをするの？ 見出しを作ったり、文章をフォーマルに見せたり、ユーザー入力の一貫性を保つためだね。

## How to: (やり方)

Pythonで文字列を大文字に変える基本的な方法を見てみよう。

```python
# 文字列の全ての文字を大文字にする
text = "hello, world!"
print(text.upper())
```

出力結果:

```
HELLO, WORLD!
```

```python
# 単語のはじめの文字だけを大文字にする
title_text = "python programming"
print(title_text.title())
```

出力結果:

```
Python Programming
```

```python
# 分かりやすさのため、新しいPython 3.6以上のf-stringも見てみよう
name = "taro"
occupation = "ninja"
print(f"{name.title()} the {occupation.title()}")
```

出力結果:

```
Taro The Ninja
```

## Deep Dive (深掘り)

大文字と小文字を区別する機能は、プログラミング言語にとって古くから存在する。文字列を大文字にするメソッド`upper()`は、Pythonが世に出た1991年からある。`title()`のようなメソッドは、ユーザーインターフェイスやデータ表示がより洗練されるにつれて重要になった。

`upper()`や`title()`は、Pythonの組み込みメソッドとして提供されるが、文化や言語によって大文字の概念がかわるケースもある。たとえば、トルコ語では、小文字の `i` にはドットなしの `İ` が大文字に対応する。このような場合、ローカライズを考慮した変換が必要になる。

実装の詳細において、`upper()`や`title()`は内部的にUnicodeデータベースを利用していて、それぞれの文字に対して適切な大文字やタイトルケースの文字を見つけ出すようになっている。

## See Also (関連情報)

- Pythonの公式ドキュメント: 文字列メソッドについてのより詳しい情報は[こちら](https://docs.python.org/3/library/stdtypes.html#string-methods)
- Unicodeについての理解を深める: 大文字小文字の変換とUnicodeの関係については[Unicode website](http://www.unicode.org/)が詳しいよ
- ローカライズに関する考慮: 異なる言語や地域の文字列操作をうまく扱うための[Pythonのlocaleモジュール](https://docs.python.org/3/library/locale.html)
