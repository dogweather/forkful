---
date: 2024-01-20 15:32:27.916405-07:00
description: "How to: (\u65B9\u6CD5) Kotlin\u3067HTML\u3092\u30D1\u30FC\u30B9\u3059\
  \u308B\u306E\u306BJsoup\u3092\u4F7F\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u7C21\
  \u5358\u306A\u30B3\u30FC\u30C9\u4F8B\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.945891-06:00'
model: unknown
summary: "(\u65B9\u6CD5) Kotlin\u3067HTML\u3092\u30D1\u30FC\u30B9\u3059\u308B\u306E\
  \u306BJsoup\u3092\u4F7F\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u7C21\u5358\u306A\
  \u30B3\u30FC\u30C9\u4F8B\u3067\u3059\u3002"
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

## How to: (方法)
KotlinでHTMLをパースするのにJsoupを使います。以下は簡単なコード例です。

```kotlin
// Jsoupライブラリをインポート
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>サンプル</title></head>" +
               "<body><p>これはパーサーの例です。</p></body></html>"
    val doc = Jsoup.parse(html)
    
    // タイトル要素を取得
    val title = doc.title()
    println("タイトル: $title")
    
    // パラグラフ要素を取得
    val p = doc.select("p").first()?.text()
    println("パラグラフ: $p")
}
```

出力:
```
タイトル: サンプル
パラグラフ: これはパーサーの例です。
```

## Deep Dive (深掘り)
HTMLパースの歴史は複雑です。初期のウェブブラウザはさまざまなHTML標準に対応するためにパーサーを内蔵し始めました。今日では、JsoupのようなライブラリはHTMLとXMLの両方をパースできる強力なツールです。

代替手段としてHTMLパースには、Kotlin/JavaではHtmlCleanerやTagSoup、PythonではBeautifulSoupやlxmlがあります。

実装の詳細に関しては、Jsoupは内部でDOM（Document Object Model）を構築しており、CSSセレクターを使って要素を簡単に検索できるようにしています。

## See Also (参照)
- Jsoup公式ドキュメント: https://jsoup.org/
- HTMLCleaner: http://htmlcleaner.sourceforge.net/
- TagSoup: http://home.ccil.org/~cowan/XML/tagsoup/
- BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/
