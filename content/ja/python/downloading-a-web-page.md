---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となぜ？

ウェブページをダウンロードするとは、そのページのHTML情報を自身のデバイスに取り込むことを意味します。これによりプログラマーは、データの分析やコンテンツの抽出などを行うことができます。

## 方法：

Pythonの人気ライブラリである`requests`を使用してウェブページをダウンロードします：

```python
import requests

url = 'https://www.python.org/'
response = requests.get(url)
print(response.text)
```

このコードの出力は、指定したURLのHTML内容となります。

## 深掘り

ウェブページのダウンロードは、初期のインターネット発展から存在しています。これは一般的にウェブスクレイピングと呼ばれ、データ収集や研究、ウェブ自動化タスクに広く使用されています。

ダウンロードのための代替手段としては、cURLやWgetなどのコマンドラインツールがあります。しかし、Pythonでこれを行う最大の利点は、ダウンロードしたデータを直接Pythonコードで操作できる点にあります。

ダウンロード操作の背後では、HTTP GETリクエストが行われており、サーバーからの応答がダウンロードしたコンテンツとなります。

## 参考情報：

さらに詳しい情報は以下のリンクから:
- Pythonのrequestsライブラリ：https://requests.readthedocs.io/en/master/
- HTTPについての詳細な情報：https://developer.mozilla.org/ja/docs/Web/HTTP/Overview
- ウェブスクレイピングについて：https://realpython.com/python-web-scraping-practical-introduction/