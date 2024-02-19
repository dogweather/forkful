---
aliases:
- /ja/ruby/downloading-a-web-page/
date: 2024-01-20 17:44:40.516647-07:00
description: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306E\u30DA\
  \u30FC\u30B8\u306E\u5185\u5BB9\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u30C7\u30FC\u30BF\u3092\u53CE\u96C6\u3001\
  \u52A0\u5DE5\u306E\u305F\u3081\u3001\u307E\u305F\u306F\u30AA\u30D5\u30E9\u30A4\u30F3\
  \u3067\u306E\u4F7F\u7528\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.390135
model: gpt-4-1106-preview
summary: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306E\u30DA\
  \u30FC\u30B8\u306E\u5185\u5BB9\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u30C7\u30FC\u30BF\u3092\u53CE\u96C6\u3001\
  \u52A0\u5DE5\u306E\u305F\u3081\u3001\u307E\u305F\u306F\u30AA\u30D5\u30E9\u30A4\u30F3\
  \u3067\u306E\u4F7F\u7528\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\
  \u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ウェブページのダウンロードとは、インターネット上のページの内容を取得することです。プログラマはデータを収集、加工のため、またはオフラインでの使用のためにこれを行います。

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
