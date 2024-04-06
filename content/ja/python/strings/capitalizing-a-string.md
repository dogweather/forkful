---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "\u65B9\u6CD5: Python \u306B\u306F\u3001\u3053\u306E\u30BF\u30B9\u30AF\
  \u3092\u7C21\u5358\u306B\u5B9F\u884C\u3059\u308B\u305F\u3081\u306E\u7D44\u307F\u8FBC\
  \u307F\u30E1\u30BD\u30C3\u30C9 `.capitalize()` \u304C\u3042\u308A\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.433680-06:00'
model: gpt-4-0125-preview
summary: "Python \u306B\u306F\u3001\u3053\u306E\u30BF\u30B9\u30AF\u3092\u7C21\u5358\
  \u306B\u5B9F\u884C\u3059\u308B\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30E1\u30BD\
  \u30C3\u30C9 `.capitalize()` \u304C\u3042\u308A\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 方法:

### Python の組み込みメソッドを使用:
Python には、このタスクを簡単に実行するための組み込みメソッド `.capitalize()` があります。

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**出力:**
```
Hello world
```

ここでは、このサイトを構築するために使用している独自の `capitalize()` を紹介します。**HTML** のような特別な単語が常にすべて大文字であるようにする必要がありました。これは [doctests](https://docs.python.org/3/library/doctest.html) のデモンストレーションも兼ねています:

```python
def capitalize(string: str) -> str:
    """
    文字列の最初の文字を大文字にする、つまり文字列を大文字化します。
    "HTML" のような特殊なケースを処理します。

    >>> capitalize("this is html, csv, xml, and http (no REPL).")
    'This is HTML, CSV, XML, and HTTP (no REPL).'

    >>> capitalize("this is json, VBA, an IDE, and yaml in the CLI.")
    'This is JSON, VBA, an IDE, and YAML in the CLI.'
    """
    return (
        string
            .capitalize()
            .replace('cli',  'CLI')
            .replace('csv',  'CSV')
            .replace('html', 'HTML')
            .replace('http', 'HTTP')
            .replace('ide',  'IDE')
            .replace('json', 'JSON')
            .replace('repl', 'REPL')
            .replace('vba',  'VBA')
            .replace('xml',  'XML')
            .replace('yaml', 'YAML')
    )

```




### 複数の単語を処理する:
文字列の各単語を大文字で始めたい場合（タイトルなど）には、`.title()` メソッドを適用できます。

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**出力:**
```
Python Programming Essentials
```

### サードパーティのライブラリを使用:
Python の標準ライブラリは基本的な文字列の大文字化に対応していますが、`textblob` のようなライブラリは、特に自然言語処理において、より洗練された制御を提供することができます。

まず、`textblob` がインストールされていることを確認します：
```bash
pip install textblob
```

その後、使用の文脈に応じて `textblob` の大文字化が異なる動作をする可能性があることを念頭に置いて、文字列の大文字化にそれを使用します：

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**出力:**
```
This is a test sentence
```

`capitalize()` と `title()` メソッドは万能に役立ちますが、`textblob` のようなライブラリを活用することで、特定のアプリケーションにおける追加の柔軟性を提供できることを覚えておいてください。
