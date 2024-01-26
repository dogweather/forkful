---
title:                "XMLの扱い方"
date:                  2024-01-26T04:33:31.251792-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-xml.md"
---

{{< edit_this_page >}}

## 何となぜ？
XMLを扱うことは、データ保管と転送のためのマークアップ言語であるXMLドキュメントの解析、作成、操作を含みます。多くのシステムがまだXML形式でデータを交換するため、そしてレガシーサポートや既存の技術との統合のために、プログラマーはこれを行います。

## 方法
Kotlinでは、解析用に組み込みの`javax.xml.parsers`を使用できます：

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
XMLドキュメントを作成するためには、`javax.xml.transform`を使用するかもしれません：

```Kotlin
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Document
import java.io.StringWriter

fun convertDocumentToString(doc: Document): String {
    val transformer = TransformerFactory.newInstance().newTransformer()
    val result = StringWriter()
    transformer.transform(DOMSource(doc), StreamResult(result))
    return result.toString()
}
```
ドキュメントの文字列への変換のサンプル出力は、単純にXMLコンテンツを文字列形式で表示することになります。

## 深掘り
XMLは90年代からウェブおよびソフトウェア開発の中核を成しており、その読みやすさと構造化された階層性で好まれています。JSONはその単純さとメッセージサイズの小ささのためにウェブサービスで人気を得ていますが、XMLはエンタープライズ環境、SOAPベースのウェブサービス、設定（Androidレイアウトファイルのような）で引き続き広く使われています。

Kotlin/Javaの組み込み機能以外にも、Simple XML SerializationやJackson XMLモジュールのようなXML処理のための様々なライブラリやAPIがあります。しかし、`javax.xml.parsers`と`javax.xml.transform`は通常、外部依存関係を追加することなくほとんどのニーズに対応します。

KotlinでXMLを扱う場合、キーとなる実装の詳細には、文字エンコーディングを適切に処理することや、XMLインジェクション攻撃を防ぐためにXMLエンティティを管理することが含まれます。XMLを解析する際には、データの整合性を確保するために名前空間の複雑さとスキーマ検証に注意してください。

## 参照
- [Kotlinドキュメント](https://kotlinlang.org/docs/reference/)
- [Java DOM ドキュメント](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Jackson XML モジュール](https://github.com/FasterXML/jackson-dataformat-xml)