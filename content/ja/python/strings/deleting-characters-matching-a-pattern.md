---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\u65B9\u6CD5: ."
lastmod: '2024-04-04T01:27:54.295778-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u306E\u524A\
  \u9664"
weight: 5
---

## 方法:

```Python
import re

# 例の文字列
text = "Hello, World! 1234"

# 数字をすべて削除
no_digits = re.sub(r'\d', '', text)
print(no_digits)  # 出力: "Hello, World! "

# 句読点を削除
no_punctuation = re.sub(r'[^\w\s]', '', text)
print(no_punctuation)  # 出力: "Hello World 1234"

# 母音を削除
no_vowels = re.sub(r'[aeiouAEIOU]', '', text)
print(no_vowels)  # 出力: "Hll, Wrld! 1234"
```

### 私が書いたカスタム関数

これを十分に頻繁に行うため、`delete()` 関数にリファクタリングしました。これは[doctests](https://docs.python.org/3/library/doctest.html)の良いデモンストレーションでもあります:

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```



## 深い洞察
テキスト中のパターンに一致する文字を削除する実践は、コンピュータサイエンスに深く根ざしており、`sed` や `grep` などの初期の Unix ツールにまで遡ります。Pythonでは、`re` モジュールがこの機能を提供し、正規表現 - テキスト処理において強力で多用途なツールを活用しています。

`re` モジュールの代替手段には以下が含まれます:
- 簡単なケースには `replace()` のような文字列メソッド。
- より複雑なパターンと、より良い Unicode サポートを提供するサードパーティのライブラリ、例えば `regex`。

内部的には、`re.sub()` を使用すると、Python インタープリタはパターンを一連のバイトコードにコンパイルし、入力テキストに直接パターンマッチングを行うステートマシンによって処理されます。この操作は、大きな文字列または複雑なパターンに対してはリソースを多く消費するため、ビッグデータ処理においてはパフォーマンスの考慮が不可欠です。

## 関連情報
- [Python `re` モジュールのドキュメンテーション](https://docs.python.org/3/library/re.html): Pythonでの正規表現についての公式ドキュメント。
- [Regular-Expressions.info](https://www.regular-expressions.info/): 正規表現についての包括的ガイド。
- [Real Python の regex チュートリアル](https://realpython.com/regex-python/): Pythonでの正規表現の実世界応用。
