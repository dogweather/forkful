---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:34.338205-07:00
description: "Go\u3067XML\u3092\u6271\u3046\u3053\u3068\u306F\u3001\u69CB\u9020\u5316\
  \u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\u305F\u3081\u306E\u6A19\u6E96\u30D5\u30A9\u30FC\
  \u30DE\u30C3\u30C8\u3067\u3042\u308BXML\u6587\u66F8\u306E\u89E3\u6790\uFF08\u8AAD\
  \u307F\u8FBC\u307F\uFF09\u3068\u751F\u6210\uFF08\u66F8\u304D\u8FBC\u307F\uFF09\u3092\
  \u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\
  \u30BF\u306E\u4FDD\u5B58\u3001\u8A2D\u5B9A\u306E\u69CB\u6210\u3001\u307E\u305F\u306F\
  \u7279\u306BXML\u304C\u597D\u307E\u3057\u3044\u307E\u305F\u306F\u30EC\u30AC\u30B7\
  \u30FC\u306A\u30C7\u30FC\u30BF\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3042\u308B\
  \u74B0\u5883\u3067\u306E\u30B7\u30B9\u30C6\u30E0\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\
  \u63DB\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.420517-06:00'
model: gpt-4-0125-preview
summary: "Go\u3067XML\u3092\u6271\u3046\u3053\u3068\u306F\u3001\u69CB\u9020\u5316\u30C7\
  \u30FC\u30BF\u4EA4\u63DB\u306E\u305F\u3081\u306E\u6A19\u6E96\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u3067\u3042\u308BXML\u6587\u66F8\u306E\u89E3\u6790\uFF08\u8AAD\u307F\
  \u8FBC\u307F\uFF09\u3068\u751F\u6210\uFF08\u66F8\u304D\u8FBC\u307F\uFF09\u3092\u542B\
  \u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\
  \u306E\u4FDD\u5B58\u3001\u8A2D\u5B9A\u306E\u69CB\u6210\u3001\u307E\u305F\u306F\u7279\
  \u306BXML\u304C\u597D\u307E\u3057\u3044\u307E\u305F\u306F\u30EC\u30AC\u30B7\u30FC\
  \u306A\u30C7\u30FC\u30BF\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3042\u308B\u74B0\
  \u5883\u3067\u306E\u30B7\u30B9\u30C6\u30E0\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\
  \u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "XML\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

## 何となぜ？

GoでXMLを扱うことは、構造化データ交換のための標準フォーマットであるXML文書の解析（読み込み）と生成（書き込み）を含みます。プログラマーはデータの保存、設定の構成、または特にXMLが好ましいまたはレガシーなデータフォーマットである環境でのシステム間のデータ交換のためにこれを行います。

## 方法：

### GoでのXMLの解析
GoでXMLを解析するには、`encoding/xml`パッケージを使用します。このパッケージは、Goの構造体へのXMLのアンマーシャル（解析）に必要なツールを提供します。例として、以下の本を表すXMLデータを考えてみましょう：

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

これを解析するには、XML構造を反映する構造体を定義します：

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

type Book struct {
    XMLName xml.Name `xml:"book"`
    ID      string   `xml:"id,attr"`
    Title   string   `xml:"title"`
    Author  string   `xml:"author"`
    Pages   int      `xml:"pages"`
}

func main() {
    data := []byte(`
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
`)

    var book Book
    err := xml.Unmarshal(data, &book)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Book: %+v\n", book)
}
```

出力：

```
Book: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### GoでのXMLの生成
Goのデータ構造からXML文書を生成するには、再び`encoding/xml`パッケージを使用します。今回はGoの構造体をXMLにマーシャルします。前の`Book`構造体を考えてみましょう：

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

func main() {
    book := &Book{
        ID:     "123",
        Title:  "Learning Go",
        Author: "John Doe",
        Pages:  359,
    }

    output, err := xml.MarshalIndent(book, "", "    ")
    if err != nil {
        panic(err)
    }

    fmt.Println(xml.Header + string(output))
}
```

出力：

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## 深く掘り下げる

XMLの冗長性と複雑さは、多くのアプリケーションにおいてJSONや他のフォーマットがより人気になる原因となりました。しかし、XMLが複雑な階層的データを表現する能力を持ち、レガシーシステムや特定のドメイン（例えば、SOAPサービス）で広く使用されていることは、その関連性を保証しています。

Goの`encoding/xml`パッケージはXMLを扱うための強力なメカニズムを提供しますが、その制限にも注意が必要です。たとえば、XMLの名前空間を扱うことは煩雑であり、より簡単な使用例に比べてXMLの仕様についての詳細な理解が必要になるかもしれません。さらに、Goの静的型付けと`encoding/xml`パッケージのマーシャリングおよびアンマーシャリングの機能は一般に効率的ですが、深くネストされた構造を扱う場合や、Goの型システムに綺麗にマッピングしないXML文書を扱う場合には、開発者は挑戦に直面するかもしれません。

ほとんどの現代のアプリケーションにとって、JSONのような代替品はよりシンプルで効率的です。しかし、レガシーシステム、特定の業界標準、または複雑なデータ表現のニーズがあるコンテキストでXMLを使用する必要がある場合、Goの標準ライブラリは仕事を成し遂げるための堅牢なツールを提供します。いつものように、データ形式の最適な選択は、アプリケーションおよび環境の具体的な要件に依存します。
