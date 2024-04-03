---
date: 2024-01-20 15:32:19.606894-07:00
description: "How to: (\u3084\u308A\u65B9) Java\u306E`jsoup`\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u4F7F\u3046\u3068HTML\u306E\u30D1\u30FC\u30B9\u304C\u7C21\u5358\u306B\
  \u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u57FA\u672C\u7684\u306A\u30B3\u30FC\
  \u30C9\u4F8B\u3092\u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.943878-06:00'
model: unknown
summary: "Java\u306E`jsoup`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3046\u3068\
  HTML\u306E\u30D1\u30FC\u30B9\u304C\u7C21\u5358\u306B\u3067\u304D\u307E\u3059\u3002\
  \u4EE5\u4E0B\u306B\u57FA\u672C\u7684\u306A\u30B3\u30FC\u30C9\u4F8B\u3092\u793A\u3057\
  \u307E\u3059."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
