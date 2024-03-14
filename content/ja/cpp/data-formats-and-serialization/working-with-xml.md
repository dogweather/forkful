---
date: 2024-01-26 04:28:36.845135-07:00
description: "XML\u3092\u6271\u3046\u3068\u306F\u3001XML\uFF08eXtensible Markup Language\u3001\
  \u62E1\u5F35\u53EF\u80FD\u30DE\u30FC\u30AF\u30A2\u30C3\u30D7\u8A00\u8A9E\uFF09\u30C7\
  \u30FC\u30BF\u306E\u89E3\u6790\u3001\u4F5C\u6210\u3001\u304A\u3088\u3073\u64CD\u4F5C\
  \u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u305D\u306E\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u4E2D\u7ACB\u306A\
  \u6027\u8CEA\u304B\u3089\u3001\u69CB\u9020\u5316\u30C7\u30FC\u30BF\u306E\u4EA4\u63DB\
  \u3084\u8A2D\u5B9A\u306A\u3069\u3092\u6271\u3046\u305F\u3081\u306BXML\u3092\u7BA1\
  \u7406\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.586554-06:00'
model: gpt-4-0125-preview
summary: "XML\u3092\u6271\u3046\u3068\u306F\u3001XML\uFF08eXtensible Markup Language\u3001\
  \u62E1\u5F35\u53EF\u80FD\u30DE\u30FC\u30AF\u30A2\u30C3\u30D7\u8A00\u8A9E\uFF09\u30C7\
  \u30FC\u30BF\u306E\u89E3\u6790\u3001\u4F5C\u6210\u3001\u304A\u3088\u3073\u64CD\u4F5C\
  \u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u305D\u306E\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u4E2D\u7ACB\u306A\
  \u6027\u8CEA\u304B\u3089\u3001\u69CB\u9020\u5316\u30C7\u30FC\u30BF\u306E\u4EA4\u63DB\
  \u3084\u8A2D\u5B9A\u306A\u3069\u3092\u6271\u3046\u305F\u3081\u306BXML\u3092\u7BA1\
  \u7406\u3057\u307E\u3059\u3002"
title: "XML\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
XMLを扱うとは、XML（eXtensible Markup Language、拡張可能マークアップ言語）データの解析、作成、および操作を意味します。プログラマーは、そのプラットフォーム中立な性質から、構造化データの交換や設定などを扱うためにXMLを管理します。

## 方法：
ここに、TinyXML-2ライブラリを使用してXMLを解析する簡単な方法があります：

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Hello, World!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

サンプル出力：

```
Hello, World!
```

そして、これがXMLファイルを作成する方法です：

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* declaration = doc.NewDeclaration();
    doc.InsertFirstChild(declaration);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* message = doc.NewElement("message");
    message->SetText("Hello, World!");
    root->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

これにより、次の内容を持つXMLファイル`output.xml`が生成されます：

```xml
<?xml version="1.0"?>
<root>
    <message>Hello, World!</message>
</root>
```

## 深掘り
XMLは90年代後半から、Webサービスやデータストレージにおいて重要な役割を果たしてきました。現在、JSONやYAMLが設定や相互運用性において一般的になっていますが、XMLは多くのエンタープライズシステムで依然として重要です。C++でのXML解析は、手動でのDOM/SAX解析という古典的な方法に感じられることがあります。幸い、TinyXML-2のようなライブラリがこれを簡素化しています。C++にはビルトインのXMLサポートがなく、TinyXML-2、pugixml、Xercesのようなライブラリが難しい部分を包摂しています。

## 参照
- TinyXML-2ドキュメント：https://leethomason.github.io/tinyxml2/
- pugixmlライブラリ：https://pugixml.org/
- Xerces-C++パーサ：https://xerces.apache.org/xerces-c/
- W3C XML仕様：https://www.w3.org/XML/
