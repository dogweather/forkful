---
date: 2024-01-26 04:27:46.966773-07:00
description: "Arduino\u3067XML\u3092\u6271\u3046\u3053\u3068\u306F\u3001\u901A\u5E38\
  \u3001Web API\u3084\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u5F97\u3089\u308C\
  \u308BXML\u30C7\u30FC\u30BF\u3092\u89E3\u6790\u3057\u3001\u64CD\u4F5C\u3059\u308B\
  \u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\u305F\u3081\u306BXML\u3092\u4F7F\
  \u7528\u3059\u308B\u30B5\u30FC\u30D3\u30B9\u3068\u7D71\u5408\u3057\u305F\u308A\u3001\
  \u30C7\u30FC\u30BF\u3092\u69CB\u9020\u5316\u3055\u308C\u305F\u4EBA\u9593\u304C\u8AAD\
  \u307F\u53D6\u308A\u53EF\u80FD\u306A\u5F62\u5F0F\u3067\u4FDD\u5B58\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.486642-07:00'
model: gpt-4-0125-preview
summary: "Arduino\u3067XML\u3092\u6271\u3046\u3053\u3068\u306F\u3001\u901A\u5E38\u3001\
  Web API\u3084\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u5F97\u3089\u308C\u308B\
  XML\u30C7\u30FC\u30BF\u3092\u89E3\u6790\u3057\u3001\u64CD\u4F5C\u3059\u308B\u3053\
  \u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\u305F\u3081\u306BXML\u3092\u4F7F\u7528\
  \u3059\u308B\u30B5\u30FC\u30D3\u30B9\u3068\u7D71\u5408\u3057\u305F\u308A\u3001\u30C7\
  \u30FC\u30BF\u3092\u69CB\u9020\u5316\u3055\u308C\u305F\u4EBA\u9593\u304C\u8AAD\u307F\
  \u53D6\u308A\u53EF\u80FD\u306A\u5F62\u5F0F\u3067\u4FDD\u5B58\u3059\u308B\u305F\u3081\
  \u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "XML\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ?
ArduinoでXMLを扱うことは、通常、Web APIや設定ファイルから得られるXMLデータを解析し、操作することを含みます。プログラマーは、データ交換のためにXMLを使用するサービスと統合したり、データを構造化された人間が読み取り可能な形式で保存するためにこれを行います。

## 方法:
`XMLWriter`ライブラリを使用してXMLを作成し、`tinyxml2`ライブラリを使用して解析します。まず、Arduino IDEのライブラリマネージャーを通じてライブラリをインストールしてください。

XMLドキュメントの作成:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // シリアルを使って出力
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hello, world!").close().close();
  xml.flush();
}

void loop() {
}
```

XML文字列のデコード:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hello, world!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

サンプル出力:

```
<greeting>
  <text>Hello, world!</text>
</greeting>
```

## 深く掘り下げる
XML、または拡張可能マークアップ言語は、ドキュメントを人間が読み取り可能かつ機械が読み取り可能な形式でエンコードするための一連のルールを定義するマークアップ言語です。1990年代後半からあり、特にプラットフォームに依存しないデータ交換が必要なさまざまな分野で広く使用されています。Arduinoの限られたメモリリソースは、PC上でのXMLの取り扱いよりも挑戦的になります。したがって、軽量なライブラリが重要となります。JSONはそのシンプルな構文と小さなフットプリントのためにデータ交換において人気を博していますが、レガシーシステムやスキーマを通じてドキュメントの検証が必要なアプリケーションの場合、特にXMLは広く使われています。Arduino XML実装の鍵はストリーム解析であり、メモリ使用量を低く保つためにドキュメントをセグメントで読み取ります。

## 参照
- [TinyXML-2 ライブラリドキュメント](https://leethomason.github.io/tinyxml2/)
- JSONデータを扱う際の代替としての [Arduino JSON ライブラリ](https://arduinojson.org/)。
- 一般的なXML学習のための [W3Schools XML チュートリアル](https://www.w3schools.com/xml/)。
- 公式のXML標準と推奨事項に関する [W3C XML 仕様](https://www.w3.org/XML/)。
