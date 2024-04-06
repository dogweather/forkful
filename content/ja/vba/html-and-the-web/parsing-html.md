---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:21.163593-07:00
description: "\u65B9\u6CD5: VBA\u3067\u306F\u3001`Microsoft HTML Object Library` \u3092\
  \u4F7F\u7528\u3057\u3066 HTML \u3092\u89E3\u6790\u3059\u308B\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\u3002VBA\u30A8\u30C7\u30A3\u30BF\u3067\u3053\u306E\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u3078\u306E\u53C2\u7167\u3092\u8FFD\u52A0\u3059\u308B\u306B\u306F\
  \u3001\u30C4\u30FC\u30EB > \u53C2\u7167\u304B\u3089 `Microsoft HTML Object Library`\u2026"
lastmod: '2024-04-05T22:37:50.160595-06:00'
model: gpt-4-0125-preview
summary: "VBA\u3067\u306F\u3001`Microsoft HTML Object Library` \u3092\u4F7F\u7528\u3057\
  \u3066 HTML \u3092\u89E3\u6790\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002VBA\u30A8\u30C7\u30A3\u30BF\u3067\u3053\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3078\u306E\u53C2\u7167\u3092\u8FFD\u52A0\u3059\u308B\u306B\u306F\u3001\u30C4\u30FC\
  \u30EB > \u53C2\u7167\u304B\u3089 `Microsoft HTML Object Library` \u3092\u9078\u629E\
  \u3057\u3066\u30C1\u30A7\u30C3\u30AF\u3057\u307E\u3059\u3002\u3053\u308C\u306B\u3088\
  \u308A\u3001HTML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u306E\u30CA\u30D3\u30B2\u30FC\
  \u30B7\u30E7\u30F3\u3068\u64CD\u4F5C\u306E\u305F\u3081\u306E\u30AF\u30E9\u30B9\u306B\
  \u30A2\u30AF\u30BB\u30B9\u3067\u304D\u307E\u3059\u3002"
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

## 方法:
VBAでは、`Microsoft HTML Object Library` を使用して HTML を解析することができます。VBAエディタでこのライブラリへの参照を追加するには、ツール > 参照から `Microsoft HTML Object Library` を選択してチェックします。これにより、HTMLドキュメントのナビゲーションと操作のためのクラスにアクセスできます。

こちらは、ファイルからHTMLドキュメントをロードし、すべてのリンク（アンカータグ）を抽出する方法を示すシンプルな例です：

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' ファイルからHTMLコンテンツをロード
    htmlFile = "C:\path\to\your\file.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' HTMLドキュメントの初期化
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' すべてのアンカータグを取得
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' すべてのアンカー要素をループして、href属性を印刷
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

このスクリプトは、HTMLファイルの内容を読み取り、`HTMLDocument` オブジェクトにロードし、すべてのアンカー要素（`<a>`タグ）を取得し、それらを反復処理して、Immediate Window に各要素の`href`属性を印刷します。

## 深掘り:
歴史的に、VBAでのHTMLの解析は、現代のウェブスクレイピングやドキュメント処理技術を直接サポートしていないため、少し扱いにくい部分がありました。Microsoft HTML Object Libraryは強力ですが、やや古く、新しい技術と比べて現代のウェブ標準をスムーズに扱うことができない場合があります。

複雑なHTML解析やウェブスクレイピングタスクについては、Beautiful SoupやScrapyといったライブラリを備えたPythonのような代替ツールや言語がしばしば推奨されます。これらの現代的なツールは、より大きな柔軟性、優れたパフォーマンスを提供し、現在のウェブ標準により適合しています。しかし、Microsoft Officeエコシステム内で作業するとき、Microsoft HTML Object Libraryを使用したVBAは貴重なスキルとして残ります。これは、ExcelやAccessなどのアプリケーションとシームレスに統合され、おなじみのVBA環境を離れることなく基本的なHTMLドキュメントの取り扱いに関わるタスクを達成するための直接的な方法を提供します。
