---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:51.315074-07:00
description: "\u65B9\u6CD5\uFF1A \u3053\u3053\u3067\u4F7F\u7528\u3055\u308C\u3066\u3044\
  \u308B\u30A2\u30D7\u30ED\u30FC\u30C1\u306F\u3001\u30B7\u30F3\u30D7\u30EB\u306A\u30E6\
  \u30FC\u30B9\u30B1\u30FC\u30B9\u306B\u52B9\u679C\u7684\u3067\u3059\u304C\u3001Basic\u2026"
lastmod: '2024-04-05T22:50:55.832366-06:00'
model: gpt-4-0125-preview
summary: "**MSXML2\u3092\u53C2\u7167\u3059\u308B**\uFF1A\u307E\u305A\u3001VBA\u30D7\
  \u30ED\u30B8\u30A7\u30AF\u30C8\u304C`Microsoft XML, v6.0`\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u53C2\u7167\u3057\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\
  \u307E\u3059\u3002VBA\u30A8\u30C7\u30A3\u30BF\u30FC\u3067\u3001\u30C4\u30FC\u30EB\
  \ > \u53C2\u7167\u8A2D\u5B9A\u306B\u79FB\u52D5\u3057\u3001`Microsoft XML, v6.0`\u3092\
  \u30C1\u30A7\u30C3\u30AF\u3057\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u7528\u3044\u305FHTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u306E\u9001\u4FE1"
weight: 45
---

## 方法：
VBAでは、`Microsoft XML, v6.0`（MSXML2）ライブラリを使用して、基本認証を伴うHTTPリクエストを送信できます。これには、資格情報をbase64でエンコードされた形式で含めるために、リクエストの`"Authorization"`ヘッダーを設定することが含まれます。以下はステップバイステップのガイドです：

1. **MSXML2を参照する**：まず、VBAプロジェクトが`Microsoft XML, v6.0`ライブラリを参照していることを確認します。VBAエディターで、ツール > 参照設定に移動し、`Microsoft XML, v6.0`をチェックします。

2. **HTTPリクエストを作成して送信する**：次のVBAコードスニペットをガイドとして使用してください。`"your_username"`と`"your_password"`を実際の資格情報に置き換え、必要に応じてURLを調整してください。

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' 実際のURLに置き換えてください
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' 応答を即時ウィンドウに出力します
    ```

3. **資格情報をbase64でエンコードする**：VBAにはbase64エンコーディングのための組み込み関数はありませんが、このカスタム`EncodeBase64`関数を使用できます：

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
    
これは、指定された基本認証資格情報で`http://example.com/api/resource`へGETリクエストを送信し、応答を出力します。

## 詳細解説
ここで使用されているアプローチは、シンプルなユースケースに効果的ですが、Basic Authentication方式に依存しており、資格情報は簡単にデコード可能な形式（base64エンコーディングは暗号化ではありません）で送信されます。HTTPSコンテキスト外での脆弱性のため、SSL/TLSのような追加のセキュリティレイヤーなしにインターネット経由で機密データを転送する場合、Basic Authenticationは推奨されません。

歴史的に、Basic Authenticationはウェブリソースへのアクセスを制御するために開発された最初の方法の一つでした。今日では、OAuth 2.0のように、新しいアプリケーションのためにはより安全かつ柔軟な認証基準が一般的に好まれます。VBAの制限とより高度な認証方法に必要な外部依存関係を考えると、開発者はしばしばVBAを内部またはセキュリティがそれほど重視されない環境で使用することがありますし、迅速にアイデアをプロトタイピングするための足がかりとして使用することもあります。

VBAでHTTPリクエストを使用する際には、MSXMLライブラリの各バージョンが異なる機能やセキュリティ基準をサポートしている可能性があることを覚えておいてください。常にアプリケーションと互換性のある最新バージョンを使用して、より良いセキュリティとパフォーマンスを確保してください。さらに、新しいプロジェクト、特に安全なHTTP通信が必要な場合にVBAを選択する際には、環境の制限や潜在的に廃止された機能も考慮してください。他のプログラミング環境や言語は、同様のタスクに対してより堅牢で、安全で、維持が容易な解決策を提供する場合があります。
