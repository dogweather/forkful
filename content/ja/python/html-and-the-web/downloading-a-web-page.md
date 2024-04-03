---
date: 2024-01-20 17:44:53.041961-07:00
description: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3059\u308B\u3063\u3066\u3069\u3046\u3044\u3046\u3053\u3068\uFF1F\u8981\u3059\
  \u308B\u306B\u3001\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u5185\u5BB9\u3092\u30A4\
  \u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u304B\u3089\u81EA\u5206\u306E\u30B3\u30F3\u30D4\
  \u30E5\u30FC\u30BF\u306B\u6301\u3063\u3066\u304F\u308B\u3053\u3068\u3060\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u304C\u3053\u308C\u3092\u884C\u3046\u7406\u7531\uFF1F\u30C7\
  \u30FC\u30BF\u53CE\u96C6\u3001\u30B3\u30F3\u30C6\u30F3\u30C4\u89E3\u6790\u3001\u307E\
  \u305F\u306F\u30D0\u30C3\u30AF\u30A2\u30C3\u30D7\u306E\u305F\u3081\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.497922-06:00'
model: gpt-4-1106-preview
summary: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3059\u308B\u3063\u3066\u3069\u3046\u3044\u3046\u3053\u3068\uFF1F\u8981\u3059\
  \u308B\u306B\u3001\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u5185\u5BB9\u3092\u30A4\
  \u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u304B\u3089\u81EA\u5206\u306E\u30B3\u30F3\u30D4\
  \u30E5\u30FC\u30BF\u306B\u6301\u3063\u3066\u304F\u308B\u3053\u3068\u3060\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u304C\u3053\u308C\u3092\u884C\u3046\u7406\u7531\uFF1F\u30C7\
  \u30FC\u30BF\u53CE\u96C6\u3001\u30B3\u30F3\u30C6\u30F3\u30C4\u89E3\u6790\u3001\u307E\
  \u305F\u306F\u30D0\u30C3\u30AF\u30A2\u30C3\u30D7\u306E\u305F\u3081\u3002."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

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
