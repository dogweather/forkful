---
title:                "ウェブページのダウンロード"
html_title:           "Python: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

何故Webページをダウンロードするかと言うと、インターネット上にある情報をオフラインで閲覧したい時や、自分の作成したプログラムでWebページの内容を解析したい時に便利です。

## How To

以下にPythonを使ったWebページのダウンロード方法を示します。まずは「urllib.request」モジュールをインポートしましょう。

```Python
import urllib.request
```

そして、以下のコードで指定したURLからWebページをダウンロードできます。例として、Googleのホームページをダウンロードしてみましょう。

```Python
url = "https://www.google.com/"
response = urllib.request.urlopen(url)
data = response.read()
print(data)
```

このコードを実行すると、GoogleのホームページのHTMLコードが表示されます。これでWebページのダウンロードは完了です！

## Deep Dive

また、より詳細にWebページのダウンロードについて知りたい場合は、以下のようにコードを改良することができます。例えば、「requests」モジュールを使うことで、より簡潔かつ柔軟なコードを書くことができます。

```Python
import requests
url = "https://www.google.com/"
response = requests.get(url)
data = response.text
print(data)
```

さらに、Webページのダウンロードだけでなく、特定の要素を取得したり、スクレイピングすることもできます。例えば、以下のコードではBeautifulSoupを使って、GoogleのロゴのURLを取得しています。

```Python
from bs4 import BeautifulSoup
url = "https://www.google.com/"
response = requests.get(url)
soup = BeautifulSoup(response.text, "html.parser")
logo = soup.find("img", {"id": "hplogo"})['src']
print(logo)
```

## See Also

- [Python公式ドキュメント](https://www.python.org/doc/)
- [Requestsモジュール公式ドキュメント](https://requests.readthedocs.io/en/master/)
- [Beautiful Soup公式ドキュメント](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)