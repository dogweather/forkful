---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:25.168509-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A\u2026"
lastmod: '2024-03-13T22:44:42.733151-06:00'
model: gpt-4-0125-preview
summary: "\u4E3B\u306B\u3001Fish\u30B7\u30A7\u30EB\u306F\u76F4\u63A5HTML\u306E\u30D1\
  \u30FC\u30B9\u3092\u610F\u56F3\u3057\u3066\u8A2D\u8A08\u3055\u308C\u3066\u3044\u307E\
  \u305B\u3093\u3002\u3057\u304B\u3057\u3001`curl`\u3001`grep`\u3001`sed`\u3001`awk`\u306E\
  \u3088\u3046\u306AUnix\u30C4\u30FC\u30EB\u3092\u7D44\u307F\u5408\u308F\u305B\u305F\
  \u308A\u3001`pup`\u3084Python\u30B9\u30AF\u30EA\u30D7\u30C8\u5185\u306E`beautifulsoup`\u306E\
  \u3088\u3046\u306A\u5C02\u9580\u7684\u306A\u30C4\u30FC\u30EB\u3092\u4F7F\u7528\u3059\
  \u308B\u306E\u306B\u9577\u3051\u3066\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\
  Fish\u30B7\u30A7\u30EB\u5185\u304B\u3089\u3053\u308C\u3089\u306E\u30C4\u30FC\u30EB\
  \u3092\u5229\u7528\u3057\u3066HTML\u3092\u30D1\u30FC\u30B9\u3059\u308B\u65B9\u6CD5\
  \u3092\u793A\u3059\u4F8B\u3067\u3059."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
