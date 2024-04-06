---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:34.338205-07:00
description: "\u65B9\u6CD5\uFF1A Go\u3067XML\u3092\u89E3\u6790\u3059\u308B\u306B\u306F\
  \u3001`encoding/xml`\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\u3057\u307E\
  \u3059\u3002\u3053\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u306F\u3001Go\u306E\u69CB\
  \u9020\u4F53\u3078\u306EXML\u306E\u30A2\u30F3\u30DE\u30FC\u30B7\u30E3\u30EB\uFF08\
  \u89E3\u6790\uFF09\u306B\u5FC5\u8981\u306A\u30C4\u30FC\u30EB\u3092\u63D0\u4F9B\u3057\
  \u307E\u3059\u3002\u4F8B\u3068\u3057\u3066\u3001\u4EE5\u4E0B\u306E\u672C\u3092\u8868\
  \u3059XML\u30C7\u30FC\u30BF\u3092\u8003\u3048\u3066\u307F\u307E\u3057\u3087\u3046\
  \uFF1A."
lastmod: '2024-04-05T22:37:49.750965-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Go\u3067XML\u3092\u89E3\u6790\u3059\u308B\u306B\u306F\
  \u3001`encoding/xml`\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\u3057\u307E\
  \u3059\u3002\u3053\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u306F\u3001Go\u306E\u69CB\
  \u9020\u4F53\u3078\u306EXML\u306E\u30A2\u30F3\u30DE\u30FC\u30B7\u30E3\u30EB\uFF08\
  \u89E3\u6790\uFF09\u306B\u5FC5\u8981\u306A\u30C4\u30FC\u30EB\u3092\u63D0\u4F9B\u3057\
  \u307E\u3059\u3002\u4F8B\u3068\u3057\u3066\u3001\u4EE5\u4E0B\u306E\u672C\u3092\u8868\
  \u3059XML\u30C7\u30FC\u30BF\u3092\u8003\u3048\u3066\u307F\u307E\u3057\u3087\u3046\
  \uFF1A."
title: "XML\u3068\u306E\u4F5C\u696D"
weight: 40
---

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
