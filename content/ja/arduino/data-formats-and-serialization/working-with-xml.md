---
date: 2024-01-26 04:27:46.966773-07:00
description: "\u65B9\u6CD5: `XMLWriter`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\
  \u3057\u3066XML\u3092\u4F5C\u6210\u3057\u3001`tinyxml2`\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3092\u4F7F\u7528\u3057\u3066\u89E3\u6790\u3057\u307E\u3059\u3002\u307E\u305A\u3001\
  Arduino IDE\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u30DE\u30CD\u30FC\u30B8\u30E3\u30FC\
  \u3092\u901A\u3058\u3066\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30B9\u30C8\
  \u30FC\u30EB\u3057\u3066\u304F\u3060\u3055\u3044\u3002 XML\u30C9\u30AD\u30E5\u30E1\
  \u30F3\u30C8\u306E\u4F5C\u6210."
lastmod: '2024-03-13T22:44:42.526248-06:00'
model: gpt-4-0125-preview
summary: "`XMLWriter`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u3066\
  XML\u3092\u4F5C\u6210\u3057\u3001`tinyxml2`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\
  \u7528\u3057\u3066\u89E3\u6790\u3057\u307E\u3059\u3002\u307E\u305A\u3001Arduino\
  \ IDE\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u30DE\u30CD\u30FC\u30B8\u30E3\u30FC\u3092\
  \u901A\u3058\u3066\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\
  \u30EB\u3057\u3066\u304F\u3060\u3055\u3044."
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

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
