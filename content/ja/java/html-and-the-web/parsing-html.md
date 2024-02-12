---
title:                "HTMLの解析"
aliases:
- /ja/java/parsing-html.md
date:                  2024-01-20T15:32:19.606894-07:00
simple_title:         "HTMLの解析"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTMLパースは、HTML文書を解析し、その情報を操作や抽出に使える形にすることです。プログラマーはこのプロセスを使って、ウェブページから必要なデータを取得したり、DOM(Document Object Model)を操作するために行います。

## How to: (やり方)
Javaの`jsoup`ライブラリを使うとHTMLのパースが簡単にできます。以下に基本的なコード例を示します。

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

public class HTMLParser {
    public static void main(String[] args) {
        String html = "<html><head><title>Example</title></head>"
                    + "<body><p>Parsed HTML into a doc.</p></body></html>";
        Document doc = Jsoup.parse(html);
        Element body = doc.body();
        
        System.out.println("Title: " + doc.title());
        System.out.println("Body: " + body.text());
    }
}
```

出力例:
```
Title: Example
Body: Parsed HTML into a doc.
```

## Deep Dive (深堀り)
HTMLパースは1990年代から行われ、初期は手作業での文字列処理が一般的でした。しかし、これはエラーが発生しやすく、保守性も低い方法です。`jsoup`や`JTidy`はJavaでHTMLを扱う場合の2つの主流なライブラリです。`jsoup`は操作がシンプルで、HTML5をサポートし、失敗に寛容(Tolerant)なパーサーとして人気があります。一方で、`JTidy`はHTMLをクリーンアップし、XHTMLまたはXML出力に変換するのに適しています。DOMに近い形式でHTMLをパースすると、プログラム的に操作がしやすく、データの抽出が効率的になります。

## See Also (関連情報)
- jsoup: https://jsoup.org/ - HTMLをパースし、DOM操作、クリーニング、抽出機能を提供。
- JTidy: http://jtidy.sourceforge.net/ - HTMLからXHTMLまたはXMLを生成。
- W3C HTML Parser: https://www.w3.org/TR/html5/syntax.html#parsing - HTMLパーサーに関するW3C規格の情報。
