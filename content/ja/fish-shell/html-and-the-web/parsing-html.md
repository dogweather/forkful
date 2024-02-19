---
aliases:
- /ja/fish-shell/parsing-html/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:25.168509-07:00
description: "HTML\u306E\u30D1\u30FC\u30B9\u306F\u3001HTML\u30B3\u30F3\u30C6\u30F3\
  \u30C4\u304B\u3089\u30C7\u30FC\u30BF\u3084\u60C5\u5831\u3092\u62BD\u51FA\u3059\u308B\
  \u3053\u3068\u3067\u3042\u308A\u3001\u30A6\u30A7\u30D6\u30C7\u30FC\u30BF\u3092\u6271\
  \u3046\u969B\u306B\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A6\u30A7\u30D6\u30B9\u30AF\u30EC\u30A4\
  \u30D4\u30F3\u30B0\u3001\u30C7\u30FC\u30BF\u30DE\u30A4\u30CB\u30F3\u30B0\u3001\u307E\
  \u305F\u306F\u81EA\u52D5\u30C6\u30B9\u30C8\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306E\
  \u305F\u3081\u306B\u3001\u30A6\u30A7\u30D6\u30B5\u30A4\u30C8\u304B\u3089\u306E\u60C5\
  \u5831\u3092\u81EA\u52D5\u7684\u306B\u62BD\u51FA\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.307374
model: gpt-4-0125-preview
summary: "HTML\u306E\u30D1\u30FC\u30B9\u306F\u3001HTML\u30B3\u30F3\u30C6\u30F3\u30C4\
  \u304B\u3089\u30C7\u30FC\u30BF\u3084\u60C5\u5831\u3092\u62BD\u51FA\u3059\u308B\u3053\
  \u3068\u3067\u3042\u308A\u3001\u30A6\u30A7\u30D6\u30C7\u30FC\u30BF\u3092\u6271\u3046\
  \u969B\u306B\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A6\u30A7\u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\
  \u30F3\u30B0\u3001\u30C7\u30FC\u30BF\u30DE\u30A4\u30CB\u30F3\u30B0\u3001\u307E\u305F\
  \u306F\u81EA\u52D5\u30C6\u30B9\u30C8\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306E\u305F\
  \u3081\u306B\u3001\u30A6\u30A7\u30D6\u30B5\u30A4\u30C8\u304B\u3089\u306E\u60C5\u5831\
  \u3092\u81EA\u52D5\u7684\u306B\u62BD\u51FA\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "HTML\u306E\u89E3\u6790"
---

{{< edit_this_page >}}

## 何となぜ？

HTMLのパースは、HTMLコンテンツからデータや情報を抽出することであり、ウェブデータを扱う際に一般的なタスクです。プログラマーは、ウェブスクレイピング、データマイニング、または自動テストなどのタスクのために、ウェブサイトからの情報を自動的に抽出するためにこれを行います。

## どのように：

主に、Fishシェルは直接HTMLのパースを意図して設計されていません。しかし、`curl`、`grep`、`sed`、`awk`のようなUnixツールを組み合わせたり、`pup`やPythonスクリプト内の`beautifulsoup`のような専門的なツールを使用するのに長けています。以下は、Fishシェル内からこれらのツールを利用してHTMLをパースする方法を示す例です。

### `curl` と `grep` を使用:
HTMLコンテンツを取得し、リンクを含む行を抽出する:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

出力:
```
/page1.html
/page2.html
...
```

### `pup`（HTMLをパースするためのコマンドラインツール）の使用:

まず、`pup`がインストールされていることを確認します。次に、タグ、ID、クラスなどによって要素を抽出するためにそれを使用できます。

```fish
curl -s https://example.com | pup 'a attr{href}'
```

出力は`grep`の例と似ており、`<a>`タグのhref属性をリストします。

### Pythonスクリプトと`beautifulsoup`を用いて:

Fish自体はネイティブにHTMLをパースすることはできませんが、Pythonスクリプトとシームレスに統合します。以下は、Pythonと`BeautifulSoup`を使用してHTMLからタイトルをパースし抽出する簡潔な例です。Python環境に`beautifulsoup4`と`requests`がインストールされていることを確認してください。

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

使用法:

```fish
parse_html 'https://example.com'
```

出力:
```
Example Domain
```

これらの方法は、シンプルなコマンドラインテキスト操作からPythonスクリプト内の`beautifulsoup`の完全なパース力まで、さまざまなユースケースと複雑さの規模に対応します。HTML構造のニーズと複雑さに応じて、直接的なUnixパイプラインやより強力なスクリプティングアプローチを選択できます。
