---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\u65B9\u6CD5: \u3053\u308C\u3092\u983B\u7E41\u306B\u884C\u3046\u306E\
  \u3067\u3001`delete()` \u95A2\u6570\u306B\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\
  \u30B0\u3057\u307E\u3057\u305F\u3002[doctests](https://docs.python.org/3/library/doctest.html)\u306E\
  \u826F\u3044\u30C7\u30E2\u30F3\u30B9\u30C8\u30EC\u30FC\u30B7\u30E7\u30F3\u306B\u3082\
  \u306A\u308A\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.435036-06:00'
model: gpt-4-0125-preview
summary: "\u3053\u308C\u3092\u983B\u7E41\u306B\u884C\u3046\u306E\u3067\u3001`delete()`\
  \ \u95A2\u6570\u306B\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3057\u307E\
  \u3057\u305F\u3002[doctests](https://docs.python.org/3/library/doctest.html)\u306E\
  \u826F\u3044\u30C7\u30E2\u30F3\u30B9\u30C8\u30EC\u30FC\u30B7\u30E7\u30F3\u306B\u3082\
  \u306A\u308A\u307E\u3059\uFF1A."
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u306E\u524A\
  \u9664"
weight: 5
---

## 方法:

```Python
import re

# 例文
text = "Hello, World! 1234"

# 数字を全て除去
no_digits = re.sub(r'\d', '', text)
print(no_digits)  # 出力: "Hello, World! "

# 句読点を除去
no_punctuation = re.sub(r'[^\w\s]', '', text)
print(no_punctuation)  # 出力: "Hello World 1234"

# 母音を除去
no_vowels = re.sub(r'[aeiouAEIOU]', '', text)
print(no_vowels)  # 出力: "Hll, Wrld! 1234"
```

### 私のカスタム関数

これを頻繁に行うので、`delete()` 関数にリファクタリングしました。[doctests](https://docs.python.org/3/library/doctest.html)の良いデモンストレーションにもなります：

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



## 深掘り
テキスト内のパターンに一致する文字を削除する習慣は、コンピュータサイエンスの深い歴史に根付いており、`sed`や`grep`といった初期のUnixツールに遡ります。Pythonでの`re`モジュールは、この機能を提供し、正規表現を利用してテキスト処理のための強力で万能なツールを実現します。

`re`モジュールの代替手段には、以下が含まれます:
- 単純なケースでは`replace()`のような文字列メソッド。
- より複雑なパターンやより良いUnicodeサポートには`regex`のようなサードパーティのライブラリ。

内部では、`re.sub()`を使用すると、Pythonインタプリタはパターンを一連のバイトコードにコンパイルし、入力テキスト上で直接パターンマッチングを行う状態機械によって処理されます。この操作は大きな文字列や複雑なパターンの場合にはリソースを多く消費するため、大規模データ処理においてパフォーマンス考慮が重要になります。

## 参照
- [Python `re`モジュールドキュメント](https://docs.python.org/3/library/re.html): Pythonの正規表現に関する公式ドキュメント。
- [Regular-Expressions.info](https://www.regular-expressions.info/): 正規表現に関する包括的ガイド。
- [PythonでのregexのReal Pythonチュートリアル](https://realpython.com/regex-python/): Pythonでの正規表現の実世界アプリケーション。
