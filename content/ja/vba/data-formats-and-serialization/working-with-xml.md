---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:15.479344-07:00
description: "\u65B9\u6CD5: XML \u3092\u6271\u3044\u59CB\u3081\u308B\u306B\u306F\u3001\
  \u901A\u5E38 `MSXML2.DOMDocument` \u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u4F7F\
  \u7528\u3057\u307E\u3059\u3002\u3053\u306E\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\
  \u30B9\u3092\u4F7F\u7528\u3059\u308B\u3068\u3001XML \u30C9\u30AD\u30E5\u30E1\u30F3\
  \u30C8\u3092\u30ED\u30FC\u30C9\u3001\u89E3\u6790\u3001\u304A\u3088\u3073\u30CA\u30D3\
  \u30B2\u30FC\u30C8\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\
  \u4E0B\u306F\u3001XML\u2026"
lastmod: '2024-03-13T22:44:41.919581-06:00'
model: gpt-4-0125-preview
summary: "XML \u3092\u6271\u3044\u59CB\u3081\u308B\u306B\u306F\u3001\u901A\u5E38 `MSXML2.DOMDocument`\
  \ \u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\
  \u3053\u306E\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u3092\u4F7F\u7528\u3059\
  \u308B\u3068\u3001XML \u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u3092\u30ED\u30FC\u30C9\
  \u3001\u89E3\u6790\u3001\u304A\u3088\u3073\u30CA\u30D3\u30B2\u30FC\u30C8\u3059\u308B\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001XML \u30D5\
  \u30A1\u30A4\u30EB\u3092\u30ED\u30FC\u30C9\u3057\u3001\u305D\u306E\u69CB\u9020\u3092\
  \u30CA\u30D3\u30B2\u30FC\u30C8\u3057\u3001\u5C5E\u6027\u3068\u30C6\u30AD\u30B9\u30C8\
  \u5185\u5BB9\u3092\u8AAD\u3080\u65B9\u6CD5\u3092\u793A\u3059\u7C21\u5358\u306A\u4F8B\
  \u3067\u3059."
title: "XML\u3068\u306E\u4F5C\u696D"
weight: 40
---

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
