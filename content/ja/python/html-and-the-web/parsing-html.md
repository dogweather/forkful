---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:54.488389-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.496668-06:00'
model: gpt-4-0125-preview
summary: "HTML\u30D1\u30FC\u30B7\u30F3\u30B0\u3068\u306F\u3001\u7279\u5B9A\u306E\u60C5\
  \u5831\u3084\u8981\u7D20\u3092\u62BD\u51FA\u3059\u308B\u305F\u3081\u306B\u30A6\u30A7\
  \u30D6\u30DA\u30FC\u30B8\u306EHTML\u30B3\u30FC\u30C9\u3092\u5206\u6790\u3059\u308B\
  \u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30A6\u30A7\
  \u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3001\u30C7\u30FC\u30BF\u30DE\u30A4\
  \u30CB\u30F3\u30B0\u3001\u307E\u305F\u306F\u30A6\u30A7\u30D6\u30B5\u30A4\u30C8\u3068\
  \u306E\u81EA\u52D5\u5316\u3055\u308C\u305F\u3084\u308A\u53D6\u308A\u306E\u305F\u3081\
  \u306E\u4E00\u822C\u7684\u306A\u4F5C\u696D\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u7684\u306B\u30A6\u30A7\u30D6\
  \u30B5\u30A4\u30C8\u3068\u5BFE\u8A71\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\
  \u62BD\u51FA\u3057\u305F\u308A\u3001\u30BF\u30B9\u30AF\u3092\u81EA\u52D5\u5316\u3057\
  \u305F\u308A\u3001\u30A6\u30A7\u30D6\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u3092\u30C6\u30B9\u30C8\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
