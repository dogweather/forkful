---
title:                "XMLの扱い方"
date:                  2024-01-26T04:27:46.966773-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-xml.md"
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