---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:47.156413-07:00
description: "TOML\u306F\u3001Tom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\
  \u4E3B\u306B\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3067\u4F7F\u7528\u3055\u308C\u308B\
  \u30C7\u30FC\u30BF\u76F4\u5217\u5316\u5F62\u5F0F\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u305D\u306E\u8AAD\u307F\u3084\u3059\u3055\u3068\u30C7\
  \u30FC\u30BF\u69CB\u9020\u3078\u306E\u7C21\u5358\u306A\u30DE\u30C3\u30D4\u30F3\u30B0\
  \u3092\u6D3B\u7528\u3057\u3066\u304A\u308A\u3001\u3053\u308C\u306B\u3088\u308AVisual\
  \ Basic for\u2026"
lastmod: '2024-03-13T22:44:41.918088-06:00'
model: gpt-4-0125-preview
summary: "TOML\u306F\u3001Tom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\u4E3B\
  \u306B\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3067\u4F7F\u7528\u3055\u308C\u308B\u30C7\
  \u30FC\u30BF\u76F4\u5217\u5316\u5F62\u5F0F\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u305D\u306E\u8AAD\u307F\u3084\u3059\u3055\u3068\u30C7\u30FC\
  \u30BF\u69CB\u9020\u3078\u306E\u7C21\u5358\u306A\u30DE\u30C3\u30D4\u30F3\u30B0\u3092\
  \u6D3B\u7528\u3057\u3066\u304A\u308A\u3001\u3053\u308C\u306B\u3088\u308AVisual Basic\
  \ for Applications\uFF08VBA\uFF09\u3092\u542B\u3080\u69D8\u3005\u306A\u30D7\u30ED\
  \u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u306E\u8A2D\u5B9A\u304C\u76F4\u63A5\u884C\u3048\u307E\u3059\u3002."
title: "TOML\u3092\u6D3B\u7528\u3059\u308B"
weight: 39
---

## 方法：
VBAでのTOMLの使用は、TOMLファイルを解析して、設定や設定をVBAプロジェクトに読み込むことを含みます。VBAにはTOMLを直接サポートする機能がないため、通常はパーサーを使用するか、VBAが簡単に扱える形式、例えばJSONやXMLにTOMLデータを変換します。ここでは、シンプルなTOML設定ファイルを手動で解析する方法を紹介します：

1. **例のTOMLファイル** (`config.toml`):
```
title = "TOML 例"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **TOMLを解析するVBAコード**：

TOMLの内容が文字列変数 `tomlStr` に読み込まれたと仮定すると、以下のVBAコードは `[database]` セクションを解析する簡素なアプローチを示しています：

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim currentSection As String
    currentSection = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim line As String
        line = Trim(lines(i))
        If InStr(line, "[") > 0 And InStr(line, "]") > 0 Then
            currentSection = Mid(line, 2, Len(line) - 2)
            Set config(currentSection) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(line, "=") > 0 Then
            Dim parts() As String
            parts = Split(line, "=")
            Dim key As String
            key = Trim(parts(0))
            Dim value As String
            value = Trim(parts(1))
            config(currentSection)(key) = value
        End If
    Next i
    
    '解析済みのデータにアクセスする例
    Debug.Print "データベースサーバー: "; config("database")("server")
End Function
```

3. **サンプル出力** (即時ウィンドウ):
```
データベースサーバー: 192.168.1.1
```

## 深堀り
開発者コミュニティにおけるTOMLの実用的な受け入れは、かつて普及していたXMLとは対照的に、よりシンプルで人が読みやすい設定ファイルへの傾向を示しています。TOMLの設計哲学は明確なセマンティクスを強調し、最小限のオーバーヘッドで直接解析を目指しています。VBAで直接TOMLを扱うには、手動で解析するか、TOMLをVBAにとってより親しみやすい形式に変換するために外部ツールを活用する必要があります。この手動解析方法は基本的なアプローチを示していますが、外部ライブラリや中間フォーマット（JSONなど）の使用は、より堅牢でエラーに強い解析戦略を提供するかもしれません。VBAがマイクロソフトオフィスと広く統合されていることを考慮すると、TOMLをJSONに変換し、VBAのネイティブJSON解析機能（該当する場合）やサードパーティーのJSONパーサーを使用することで、よりシームレスなワークフローを実現できるでしょう。また、データ直列化形式の継続的な進化を踏まえると、TOMLと同様に人間の可読性を重視しながらも、複雑さと柔軟性の観点で異なるトレードオフを提供するYAMLもプログラマーは検討すべきです。
