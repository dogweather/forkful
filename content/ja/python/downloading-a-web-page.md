---
title:                "Python: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ
ウェブページをダウンロードする理由は様々です。例えば、ウェブスクレイピングやデータ収集のために必要だったり、オフラインでコンテンツを閲覧したりするために必要だったりします。

## 方法
ウェブページをダウンロードするには、Pythonの `requests` ライブラリを使用します。以下のコード例を参考にしてください。

```Python
import requests

# ダウンロードしたいページのURLを指定する
url = "https://example.com"

# requestsモジュールを使ってページをダウンロードし、responseオブジェクトに格納する
response = requests.get(url)

# responseからのコンテンツをテキスト形式で取得する
content = response.text

# ファイルに保存する
with open("page.html", "w", encoding="utf-8") as f:
    f.write(content)

# ダウンロードが成功したかどうかを確認する
if response.status_code == 200:
    print("ダウンロードが完了しました。")
else:
    print("ダウンロードに失敗しました。")
```

## ディープダイブ
ウェブページをダウンロードする際には、`get()` メソッド以外にも `post()` や `head()` メソッドなども使うことができます。また、`requests` ライブラリの他にも `urllib` や `urllib2` モジュールなどでもダウンロードが可能です。しかし、`requests` ライブラリはシンプルで使いやすく、多くの機能を提供しているので、初心者にもお勧めです。

## 参考リンク
- [Requests: HTTP for Humans (公式ドキュメント)](https://requests.readthedocs.io/)
- [PythonでHTTP通信をする方法 (Qiita)](https://qiita.com/kenichi_cc/items/adeb66752abf6970d916)
- [Scraping the Web With Python (Real Python)](https://realpython.com/python-web-scraping-practical-introduction/)