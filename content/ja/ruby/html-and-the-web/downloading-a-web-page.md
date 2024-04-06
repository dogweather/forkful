---
date: 2024-01-20 17:44:40.516647-07:00
description: "How to: (\u65B9\u6CD5) Ruby\u3067\u306F`net/http`\u3092\u5229\u7528\u3057\
  \u3066\u7C21\u5358\u306B\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\
  \u30ED\u30FC\u30C9\u3067\u304D\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.642687-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Ruby\u3067\u306F`net/http`\u3092\u5229\u7528\u3057\u3066\u7C21\
  \u5358\u306B\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3067\u304D\u307E\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## How to: (方法)
Rubyでは`net/http`を利用して簡単にウェブページをダウンロードできます。

```ruby
require 'net/http'
require 'uri'

url = URI.parse('http://www.example.com/index.html')
response = Net::HTTP.get_response(url)

if response.is_a?(Net::HTTPSuccess)
  File.write('example.html', response.body)
  puts 'Downloaded and saved as example.html'
else
  puts 'Download failed'
end
```
実行結果:
```
Downloaded and saved as example.html
```

## Deep Dive (深掘り)
もともとウェブページのダウンロードは、ブラウザが自動的に行うものでした。しかしAPIとウェブのデータが増えたため、バックエンドやスクリプトからウェブページをダウンロードする必要が出てきました。`net/http`はRuby標準ライブラリで、複雑な設定なしにHTTPリクエストができます。もし高度な機能が必要ならば、`httparty`や`rest-client`のようなサードパーティのgemも検討する価値があります。これらはレスポンス処理や例外管理に関する更に柔軟なオプションを提供します。

## See Also (関連情報)
- `httparty` gem: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- `rest-client` gem: [https://github.com/rest-client/rest-client](https://github.com/rest-client/rest-client)
- Rubyプログラミング言語の公式サイト: [https://www.ruby-lang.org/ja/](https://www.ruby-lang.org/ja/)
