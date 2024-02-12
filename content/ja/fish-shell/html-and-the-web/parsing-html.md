---
title:                "HTMLの解析"
aliases: - /ja/fish-shell/parsing-html.md
date:                  2024-02-03T19:12:25.168509-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
