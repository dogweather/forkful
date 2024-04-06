---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:13.326794-07:00
description: "\u65B9\u6CD5\uFF1A VBA \u3067 HTTP \u30EA\u30AF\u30A8\u30B9\u30C8\u3092\
  \u9001\u4FE1\u3059\u308B\u9375\u306F\u3001`Microsoft XML, v6.0` \u30E9\u30A4\u30D6\
  \u30E9\u30EA\uFF08\u307E\u305F\u306F\u30B7\u30B9\u30C6\u30E0\u306B\u5FDC\u3058\u3066\
  \u53E4\u3044\u30D0\u30FC\u30B8\u30E7\u30F3\uFF09\u306E\u5229\u7528\u3067\u3059\u3002\
  \u307E\u305A\u3001VBA \u30A8\u30C7\u30A3\u30BF\u306E\u30C4\u30FC\u30EB > \u53C2\u7167\
  \ \u306B\u79FB\u52D5\u3057\u3001`Microsoft XML, v6.0` \u304C\u6709\u52B9\u306B\u306A\
  \u3063\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\
  \u3044\u3002\u2026"
lastmod: '2024-04-05T22:37:50.158874-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A VBA \u3067 HTTP \u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\
  \u4FE1\u3059\u308B\u9375\u306F\u3001`Microsoft XML, v6.0` \u30E9\u30A4\u30D6\u30E9\
  \u30EA\uFF08\u307E\u305F\u306F\u30B7\u30B9\u30C6\u30E0\u306B\u5FDC\u3058\u3066\u53E4\
  \u3044\u30D0\u30FC\u30B8\u30E7\u30F3\uFF09\u306E\u5229\u7528\u3067\u3059\u3002\u307E\
  \u305A\u3001VBA \u30A8\u30C7\u30A3\u30BF\u306E\u30C4\u30FC\u30EB > \u53C2\u7167\
  \ \u306B\u79FB\u52D5\u3057\u3001`Microsoft XML, v6.0` \u304C\u6709\u52B9\u306B\u306A\
  \u3063\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\
  \u3044\u3002 \u3053\u3053\u306B\u3001\u5358\u7D14\u306A HTTP GET \u30EA\u30AF\u30A8\
  \u30B9\u30C8\u3092\u9001\u4FE1\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\
  \uFF1A."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

## 方法：
VBA で HTTP リクエストを送信する鍵は、`Microsoft XML, v6.0` ライブラリ（またはシステムに応じて古いバージョン）の利用です。まず、VBA エディタのツール > 参照 に移動し、`Microsoft XML, v6.0` が有効になっていることを確認してください。

ここに、単純な HTTP GET リクエストを送信する方法を示します：

```vb
Dim httpRequest As Object
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")

With httpRequest
    .Open "GET", "https://api.example.com/data", False
    .send
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Error: " & .Status & " - " & .statusText
    End If
End With
```

データ（例えば、JSON）をサーバーに送信する必要がある POST リクエストの場合は以下の通りです：

```vb
Dim httpRequest As Object, postData As String
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")
postData = "{""key"":""value""}"

With httpRequest
    .Open "POST", "https://api.example.com/submit", False
    .setRequestHeader "Content-Type", "application/json"
    .send postData
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Error: " & .Status & " - " & .statusText
    End If
End With
```

成功したリクエストのサンプル出力は、対話している API や Web ページに応じて、JSON 文字列または HTML ページになることがあります：

```
{"data": "This is the response from the server"}
```

## 深掘り
ここで紹介した方法は、`MSXML2.XMLHTTP` オブジェクトを利用します。これは Microsoft XML Core Services (MSXML) の一部であり、VBA 開発者が XML ベースの操作を行うための方法を提供するために導入され、時間が経つにつれて、直接 XML データを扱わない場合でも HTTP リクエストのための一般的なツールとなりました。古いものであるにも関わらず、VBA における単純な Web インタラクションにおける信頼性のあるオプションとして残っています。

しかしながら、VBA およびその HTTP リクエストメカニズムは、現代のプログラミング環境に見られる堅牢さや柔軟性に欠けています。例えば、非同期リクエストの処理や、高度な HTTP 機能（ウェブソケットやサーバー送信イベントなど）を要求するアプリケーション内での作業は、VBA の範囲外です。より複雑な Web 統合プロジェクトに取り組む際、開発者はしばしば外部ライブラリやツールを利用したり、ウェブスクレイピング技術を通じてブラウザの挙動を自動化したりしますが、これらは解決策というよりは回避策です。

Python の `requests` ライブラリや Node.js 上で動作する JavaScript のような言語や環境は、非同期操作、JSON の扱いの容易さ、さまざまな Web 技術に対する広範なサポートを含む、より強力で多様な HTTP リクエスト機能を箱から出してすぐに提供します。より洗練された Web インタラクションを必要とするタスクには、Microsoft のエコシステムに深く根ざした開発者は、.NET の広範なネットワークプログラミング機能を活用するために PowerShell や C# への移行を検討するかもしれません。

したがって、VBA の HTTP リクエスト機能は単純なクエリやデータ取得タスクには適していますが、プロジェクトの要求が複雑で現代的な Web 環境に向かうにつれて、代替案を探求することが重要になります。
