---
title:                "ウェブページのダウンロード"
date:                  2024-01-20T17:44:53.041961-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ?)

ウェブページをダウンロードするってどういうこと？要するに、ウェブページの内容をインターネットから自分のコンピュータに持ってくることだ。プログラマがこれを行う理由？データ収集、コンテンツ解析、またはバックアップのため。

## How to: (方法)

```Python
import requests

# ウェブページのURL
url = 'http://example.com'

# リクエストを送り、レスポンスを取得
response = requests.get(url)

# ウェブページの内容を確認
content = response.text

print(content)  # ターミナルに出力する、または必要ならファイルに保存する
```

サンプル出力:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
<body>
    <div>
        <h1>Example Domain</h1>
        <p>This domain is for use in illustrative examples in documents...</p>
        ...
    </div>
</body>
</html>
```

## Deep Dive (詳細情報)

ウェブページのダウンロードは古くから行われている。様々な方法があるけど、代表的なのは`requests`ライブラリを使う方法だ。`urllib`のような古いライブラリと比べ、`requests`はシンプルで扱いやすい。

イレギュラーなケースには、`session`オブジェクトを使って状態を保持したり、エラーハンドリングのために`try-except`ブロックを使うなど工夫が必要。

ウェブスクレイピングの法的側面にも注意。ダウンロードは公開データに限るべきで、サーバに負担をかけないように配慮しなくてはならない。

## See Also (関連情報)

- `requests` documentation: https://requests.readthedocs.io/en/latest/
- Beautiful Soup for parsing HTML: https://www.crummy.com/software/BeautifulSoup/
- Legal aspects of web scraping: https://www.eff.org/issues/coders/computer-programmers-and-reverse-engineering

これらのリンクで、もっと学べる。ウェブページを正しく、責任を持って扱おう。
