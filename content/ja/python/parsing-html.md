---
title:                "HTMLの解析"
aliases:
- ja/python/parsing-html.md
date:                  2024-02-03T19:12:54.488389-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
HTMLパーシングとは、特定の情報や要素を抽出するためにウェブページのHTMLコードを分析することを指します。これは、ウェブスクレイピング、データマイニング、またはウェブサイトとの自動化されたやり取りのための一般的な作業です。プログラマーは、プログラム的にウェブサイトと対話したり、データを抽出したり、タスクを自動化したり、ウェブアプリケーションをテストしたりするためにこれを行います。

## 方法:
Pythonは、ウェブスクレイピングやHTMLパーシングのために、BeautifulSoupやrequestsのような強力なライブラリを提供しています。始めるには、まだインストールしていなければこれらのライブラリをインストールする必要があります：

```bash
pip install beautifulsoup4 requests
```

以下は、`requests`を使用してウェブページのHTMLコンテンツを取得し、`BeautifulSoup`でそれを解析する基本的な例です：

```python
import requests
from bs4 import BeautifulSoup

# ウェブページのコンテンツを取得
URL = 'https://example.com'
page = requests.get(URL)

# HTMLコンテンツを解析
soup = BeautifulSoup(page.content, 'html.parser')

# ウェブページのタイトルを抽出する例
title = soup.find('title').text
print(f'ウェブページのタイトル: {title}')
```

**サンプル出力**:
```
ウェブページのタイトル: Example Domain
```

ウェブページからすべてのリンクを抽出するなど、より複雑なクエリに対しては、BeautifulSoupのパースツリーをナビゲートして検索するためのさまざまなメソッドを使用できます：

```python
# <a>タグ内のすべてのリンクを抽出
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**サンプル出力**:
```
https://www.iana.org/domains/example
```

BeautifulSoupの柔軟性は、必要な正確なデータを検索するために検索をカスタマイズすることを可能にし、HTMLパーシングをウェブコンテンツを扱うプログラマーにとって強力なツールにします。
