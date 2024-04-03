---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:58.404584-07:00
description: "Visual Basic for\u2026"
lastmod: '2024-03-13T22:44:41.882743-06:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications\uFF08VBA\uFF09\u3067\u30A6\u30A7\u30D6\u30DA\
  \u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\u3053\u3068\u306F\
  \u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u304B\u3089\u30A6\u30A7\u30D6\u30DA\
  \u30FC\u30B8\u306EHTML\u5185\u5BB9\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u3092\
  \u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3057\u3070\
  \u3057\u3070\u3001Excel\u3001Access\u3001\u307E\u305F\u306F\u4ED6\u306EOffice\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u304B\u3089\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u7684\u306B\u30A6\u30A7\u30D6\u30B5\u30A4\u30C8\u306E\u5185\u5BB9\u3092\
  \u51E6\u7406\u307E\u305F\u306F\u5206\u6790\u3059\u308B\u305F\u3081\u306B\u3053\u306E\
  \u4F5C\u696D\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## 方法
VBAでウェブページをダウンロードするには、サーバーHTTPリクエストを有効にするMicrosoft XML、v6.0（MSXML6）ライブラリを利用できます。コードに入る前に、VBAエディタで `ツール` -> `参照設定` に進んで、`Microsoft XML, v6.0`がチェックされていることを確認しましょう。

ウェブページのHTML内容をダウンロードする簡単な例をこちらに示します：

```basic
Sub DownloadWebPage()
    Dim request As Object
    Dim url As String
    Dim response As String
    
    ' XML HTTPリクエストオブジェクトを初期化する
    Set request = CreateObject("MSXML2.XMLHTTP")
    
    url = "http://www.example.com"
    
    ' 同期リクエストを開く
    request.Open "GET", url, False
    
    ' サーバーへリクエストを送信する
    request.send
    
    ' レスポンステキストを取得する
    response = request.responseText
    
    ' レスポンスを即時ウィンドウに出力する（デバッグ目的用）
    Debug.Print response
    
    ' クリーンアップ
    Set request = Nothing
End Sub
```

このサブルーチンを実行すると、VBAエディタの即時ウィンドウに`http://www.example.com`のHTMLが出力されます。`Open`メソッドの`False`パラメータはリクエストを同期的にすることを意味しており、次の行へ移動する前にウェブページがダウンロードされるまでコードが待機することを意味します。

## 深掘り
示されている技術は、MSXMLに依存しています。これは、MicrosoftのXML HTTP Request標準の実装であり、ウェブ開発でAJAXリクエストにしばしば使用されます。このコンポーネントは長い間Microsoftの技術スタックの一部であり、VBAでのネットワークリクエストにとって堅牢な選択肢です。

しかし、VBAとMSXMLへの依存は、JavaScriptを大量に使用し動的なコンテンツのレンダリングを行う現代のウェブアプリケーションで制限が発生する可能性があります。これらの制限は、JavaScriptを実行し複雑なウェブサイトの相互作用を扱う能力を持つ、Pythonのような他の言語やBeautifulSoupやSeleniumなどのライブラリがウェブスクレイピングタスクにより適している理由です。

それにもかかわらず、単純なタスクで直接HTML内容を取得する場合や、Officeアプリケーションの制約内で作業する場合には、VBAは実用的なツールのままです。Officeスイート内での統合は、ウェブコンテンツに基づいてドキュメントを直接操作するための独自の利点を提供し、特定のユースケースには便利です。
