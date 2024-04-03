---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:54.488389-07:00
description: "\u65B9\u6CD5: Python\u306F\u3001\u30A6\u30A7\u30D6\u30B9\u30AF\u30EC\
  \u30A4\u30D4\u30F3\u30B0\u3084HTML\u30D1\u30FC\u30B7\u30F3\u30B0\u306E\u305F\u3081\
  \u306B\u3001BeautifulSoup\u3084requests\u306E\u3088\u3046\u306A\u5F37\u529B\u306A\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\
  \u59CB\u3081\u308B\u306B\u306F\u3001\u307E\u3060\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\
  \u3057\u3066\u3044\u306A\u3051\u308C\u3070\u3053\u308C\u3089\u306E\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3059\u308B\u5FC5\u8981\u304C\
  \u3042\u308A\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.496668-06:00'
model: gpt-4-0125-preview
summary: "Python\u306F\u3001\u30A6\u30A7\u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\
  \u30B0\u3084HTML\u30D1\u30FC\u30B7\u30F3\u30B0\u306E\u305F\u3081\u306B\u3001BeautifulSoup\u3084\
  requests\u306E\u3088\u3046\u306A\u5F37\u529B\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u59CB\u3081\u308B\u306B\u306F\
  \u3001\u307E\u3060\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u3066\u3044\u306A\u3051\
  \u308C\u3070\u3053\u308C\u3089\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\
  \u30B9\u30C8\u30FC\u30EB\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\uFF1A\
  ."
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
