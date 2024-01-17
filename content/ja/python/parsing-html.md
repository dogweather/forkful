---
title:                "HTMLのパース"
html_title:           "Python: HTMLのパース"
simple_title:         "HTMLのパース"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/parsing-html.md"
---

{{< edit_this_page >}}

何 & なぜ？

HTML解析とは、ウェブページのHTMLコードを分析し、必要な情報を抽出するプロセスです。開発者たちはHTML解析を行うことで、ウェブスクレイピングやウェブデータマイニングなどのタスクを自動化することができます。

やり方：

Pythonの標準ライブラリには、HTMLを解析するための便利なモジュールがあります。例えば、```html.parser```モジュールを使うことで、HTMLのタグや属性を簡単に取得できます。以下に、簡単なコード例を示します。

```
# 必要なモジュールをインポートする
from urllib.request import urlopen
from bs4 import BeautifulSoup

# 解析したいウェブページのURLを指定する
url = "https://www.example.com"

# ウェブページのHTMLを取得し、BeautifulSoupオブジェクトに変換する
html = urlopen(url).read()
soup = BeautifulSoup(html, "html.parser")

# h1タグのテキストを取得する
title = soup.find("h1").text

# テキストを出力する
print(title)
```

出力結果：
```
Example Domain
```

ディープダイブ：

HTML解析の歴史は、1990年代初頭のWorld Wide Webの登場とともに始まりました。当時、ウェブページは単純なテキストではなく、マークアップ言語であるHTMLで書かれていました。このため、ウェブページの情報を抽出するためには、HTML解析が必要でした。

HTML解析には、Python以外にも多くの言語で実装されています。例えば、Javaの「Jsoup」やJavaScriptの「Cheerio」などがあります。しかし、Pythonのライブラリが豊富で使いやすいため、開発者にとってはHTML解析にPythonを選ぶことが一般的です。

参考リンク：

- Beautiful Soup documentation: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Jsoup: https://jsoup.org/
- Cheerio: https://cheerio.js.org/