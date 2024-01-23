---
title:                "HTMLの解析"
date:                  2024-01-20T15:33:35.800580-07:00
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTMLパースは、ウェブページからデータを抽出することです。プログラマーはHTMLをパースしてデータを得たり、コンテンツを解析したりするために行います。

## How to: (方法)
PythonでHTMLをパースする基本的な方法は、`BeautifulSoup`と`requests`ライブラリを使うことです。

```Python
from bs4 import BeautifulSoup
import requests

# ウェブページを取得
url = 'http://example.com'
response = requests.get(url)

# BeautifulSoupオブジェクトを作成
soup = BeautifulSoup(response.text, 'html.parser')

# タイトルを抽出
title = soup.find('title').text
print(f'ページのタイトルは「{title}」です。')
```

実行結果:
```
ページのタイトルは「Example Domain」です。
```

## Deep Dive (深掘り)
HTMLのパースは1990年代から行われています。初期は正規表現などの基本的なテキスト処理で行っていましたが、複雑さを理解するためにDOMツリーを解析するライブラリが開発されました。`BeautifulSoup`はそのうちの一つで、その使いやすさから人気があります。他の選択肢には`lxml`や`html.parser`(Python標準ライブラリ)が存在します。HTML構造は複雑で予測不可能なため、これらのツールは強力なエラー処理機能を持っている必要があります。

## See Also (関連情報)
- BeautifulSoupのドキュメント: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Python `requests`ライブラリ: https://requests.readthedocs.io/en/master/
- Python標準ライブラリ `html.parser`: https://docs.python.org/3/library/html.parser.html
- `lxml`ライブラリ: https://lxml.de/
