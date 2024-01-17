---
title:                "HTMLの解析"
html_title:           "C#: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Parsing HTML とは?
HTMLのパースとは、HTML文書を解析して、その構成要素や情報を読み取ることです。プログラマーがこれを行う理由は、ウェブサイトやアプリケーションでHTMLを使って表示する際に、特定の部分を抜き出したり、変更したりする必要があるためです。

## どのように行うか:
以下は、HTMLをパースするC#のコード例です。```C#
// HTMLを取得する
string html = "<html><body><h1>Hello World!</h1></body></html>";

// HtmlAgilityPackライブラリを使用して、HTMLドキュメントオブジェクトを作成する
HtmlAgilityPack.HtmlDocument doc = new HtmlAgilityPack.HtmlDocument();
doc.LoadHtml(html);

// XPathを使用して、指定したタグのテキストを取得する
string title = doc.DocumentNode.SelectSingleNode("//h1").InnerText; // 結果: Hello World!

// テキストを変更する
title = "Hello Universe!"; // 結果: Hello Universe!

// HTMLを再構築する
string newHtml = doc.DocumentNode.OuterHtml; // 結果: <html><body><h1>Hello Universe!</h1></body></html>

## 詳細な説明:
### 歴史的背景
HTMLの最初のバージョンは、1993年にティモシー・バーナーズ＝リーによって開発されました。その後、さまざまなバージョンがリリースされ、現在ではHTML5が最新のバージョンです。HTMLのパースは、この歴史の中で発展してきました。

### 代替手段
HTMLをパースするために、C#の他にも多くの言語やツールがあります。代表的なものとしては、PythonのBeautiful Soup、JavaScriptのCheerio、そしてPHPのDOMDocumentが挙げられます。それぞれの言語やツールによって、パースの方法や機能に差がありますので、自分に合ったものを選ぶことが重要です。

### 実装の詳細
上記のコード例では、HtmlAgilityPackというライブラリを使用してHTMLをパースしています。このライブラリを使用すると、XPathを使用してHTMLの特定の部分を選択することができます。また、パースしたHTMLを再構築することも可能です。詳細な使い方は、公式ドキュメントを参照することをおすすめします。

## 関連リンク:
- [HtmlAgilityPack公式サイト](https://html-agility-pack.net/)
- [Cheerio公式サイト](https://cheerio.js.org/)
- [DOMDocument公式ドキュメント](https://www.php.net/manual/en/class.domdocument.php)