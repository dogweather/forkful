---
date: 2024-01-20 15:32:19.606894-07:00
description: "HTML\u30D1\u30FC\u30B9\u306F\u3001HTML\u6587\u66F8\u3092\u89E3\u6790\
  \u3057\u3001\u305D\u306E\u60C5\u5831\u3092\u64CD\u4F5C\u3084\u62BD\u51FA\u306B\u4F7F\
  \u3048\u308B\u5F62\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u3092\u4F7F\u3063\u3066\
  \u3001\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u304B\u3089\u5FC5\u8981\u306A\u30C7\u30FC\
  \u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001DOM(Document Object Model)\u3092\
  \u64CD\u4F5C\u3059\u308B\u305F\u3081\u306B\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.943878-06:00'
model: unknown
summary: "HTML\u30D1\u30FC\u30B9\u306F\u3001HTML\u6587\u66F8\u3092\u89E3\u6790\u3057\
  \u3001\u305D\u306E\u60C5\u5831\u3092\u64CD\u4F5C\u3084\u62BD\u51FA\u306B\u4F7F\u3048\
  \u308B\u5F62\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u3092\u4F7F\u3063\u3066\u3001\
  \u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u304B\u3089\u5FC5\u8981\u306A\u30C7\u30FC\u30BF\
  \u3092\u53D6\u5F97\u3057\u305F\u308A\u3001DOM(Document Object Model)\u3092\u64CD\
  \u4F5C\u3059\u308B\u305F\u3081\u306B\u884C\u3044\u307E\u3059\u3002"
title: "HTML\u306E\u89E3\u6790"
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
