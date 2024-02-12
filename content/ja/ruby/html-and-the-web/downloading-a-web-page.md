---
title:                "ウェブページのダウンロード"
aliases:
- /ja/ruby/downloading-a-web-page/
date:                  2024-01-20T17:44:40.516647-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/downloading-a-web-page.md"
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
