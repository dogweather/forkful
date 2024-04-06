---
date: 2024-01-20 17:44:53.041961-07:00
description: "How to: (\u65B9\u6CD5) \u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\
  \u30A6\u30F3\u30ED\u30FC\u30C9\u306F\u53E4\u304F\u304B\u3089\u884C\u308F\u308C\u3066\
  \u3044\u308B\u3002\u69D8\u3005\u306A\u65B9\u6CD5\u304C\u3042\u308B\u3051\u3069\u3001\
  \u4EE3\u8868\u7684\u306A\u306E\u306F`requests`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\
  \u4F7F\u3046\u65B9\u6CD5\u3060\u3002`urllib`\u306E\u3088\u3046\u306A\u53E4\u3044\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u3068\u6BD4\u3079\u3001`requests`\u306F\u30B7\u30F3\
  \u30D7\u30EB\u3067\u6271\u3044\u3084\u3059\u3044\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:55.502166-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\
  \u30ED\u30FC\u30C9\u306F\u53E4\u304F\u304B\u3089\u884C\u308F\u308C\u3066\u3044\u308B\
  \u3002\u69D8\u3005\u306A\u65B9\u6CD5\u304C\u3042\u308B\u3051\u3069\u3001\u4EE3\u8868\
  \u7684\u306A\u306E\u306F`requests`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3046\
  \u65B9\u6CD5\u3060\u3002`urllib`\u306E\u3088\u3046\u306A\u53E4\u3044\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u3068\u6BD4\u3079\u3001`requests`\u306F\u30B7\u30F3\u30D7\u30EB\
  \u3067\u6271\u3044\u3084\u3059\u3044\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

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
