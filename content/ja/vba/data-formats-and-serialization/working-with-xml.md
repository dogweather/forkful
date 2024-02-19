---
aliases:
- /ja/vba/working-with-xml/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:15.479344-07:00
description: "Visual Basic for Applications (VBA) \u3067 XML \u3092\u6271\u3046\u3053\
  \u3068\u306F\u3001Microsoft Office \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u306E\u6587\u8108\u5185\u3067 XML \u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u3092\u89E3\
  \u6790\u3001\u4F5C\u6210\u3001\u304A\u3088\u3073\u5909\u66F4\u3059\u308B\u3053\u3068\
  \u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  Office \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3092 XML \u3092\u51FA\u529B\
  \u3059\u308B Web\u2026"
lastmod: 2024-02-18 23:08:54.784769
model: gpt-4-0125-preview
summary: "Visual Basic for Applications (VBA) \u3067 XML \u3092\u6271\u3046\u3053\u3068\
  \u306F\u3001Microsoft Office \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\
  \u6587\u8108\u5185\u3067 XML \u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u3092\u89E3\u6790\
  \u3001\u4F5C\u6210\u3001\u304A\u3088\u3073\u5909\u66F4\u3059\u308B\u3053\u3068\u3092\
  \u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001Office\
  \ \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3092 XML \u3092\u51FA\u529B\u3059\
  \u308B Web\u2026"
title: "XML\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

## 何となぜ?

Visual Basic for Applications (VBA) で XML を扱うことは、Microsoft Office アプリケーションの文脈内で XML ドキュメントを解析、作成、および変更することを含みます。プログラマーは、Office アプリケーションを XML を出力する Web サービスや他のデータソースと統合し、データ交換やレポーティング機能を容易にするために、この機能を利用します。

## 方法:

XML を扱い始めるには、通常 `MSXML2.DOMDocument` オブジェクトを使用します。このインターフェースを使用すると、XML ドキュメントをロード、解析、およびナビゲートすることができます。以下は、XML ファイルをロードし、その構造をナビゲートし、属性とテキスト内容を読む方法を示す簡単な例です。

```basic
' まず、ツール -> 参照を通して "Microsoft XML, v6.0" への参照を追加したことを確認してください
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Path\To\Your\File.xml") ' あなたの XML ファイルをロード

' XML が正常にロードされたかを確認
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "XML ロードエラー:" & xmlDoc.parseError.reason
Else
    '  要素をナビゲートして読む
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' <book>  内の最初の <title> を見つけるための XPath
    MsgBox book.Text ' タイトルテキストを表示
End If
```

上記のサンプルコードでは、`MSXML2.DOMDocument60` のインスタンスを作成し、XML ファイルをロードしてから、エラーをチェックします。エラーが見つからない場合は、XPath を使用して特定のノードにナビゲートし、そのテキスト内容を表示します。

## 深堀り:

VBA における XML 機能の統合は、Office アプリケーションが Web データおよびサービスとやり取りする必要性が高まった2000 年代初頭にさかのぼります。`MSXML` ライブラリ、または Microsoft XML Core Services は年々進化し、改善されたパフォーマンスとセキュリティ機能を備える最新バージョンとして `MSXML2.DOMDocument60` が推奨されています。

しかし、VBA の XML 処理機能は強力であるにもかかわらず、Python の XML.etree や C#'s LINQ to XML など、現代のプログラミング環境に比べて効率が低く、より煩雑であると考えられています。VBA の固有の冗長性と参照を手動で追加および管理する必要性が、迅速な開発を妨げる可能性があります。さらに、JSON がより軽量なデータ交換フォーマットとして登場して以来、伝統的なシステムや特定のエンタープライズサービスとの互換性を必要とする場合を除き、多くのプログラマーやアプリケーションが XML から離れています。

しかしながら、Microsoft Office の自動化の文脈で XML ドキュメントの解析や生成が必要なタスクにおいて、VBA の XML 処理機能を活用することは、実行可能であり、時には必要なアプローチです。これにより、Office アプリケーションの豊富な機能セットと XML によって提供される構造化されたデータ操作機能へのアクセスのバランスが取れます。
