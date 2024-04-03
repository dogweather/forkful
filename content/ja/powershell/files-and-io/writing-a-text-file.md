---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:09.279202-07:00
description: "\u65B9\u6CD5: PowerShell\u306F\u30D5\u30A1\u30A4\u30EB\u51E6\u7406\u306E\
  \u305F\u3081\u306E\u76F4\u611F\u7684\u306A\u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\
  \u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002`Out-File` \u30B3\u30DE\u30F3\
  \u30C9\u30EC\u30C3\u30C8\u3068\u30EA\u30C0\u30A4\u30EC\u30AF\u30B7\u30E7\u30F3\u6F14\
  \u7B97\u5B50\u306F\u3001\u3053\u306E\u76EE\u7684\u306E\u305F\u3081\u306B\u4E3B\u306B\
  \u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u7570\u306A\u308B\
  \u30B7\u30CA\u30EA\u30AA\u3067\u30D5\u30A1\u30A4\u30EB\u306B\u30C6\u30AD\u30B9\u30C8\
  \u3092\u66F8\u304D\u8FBC\u3080\u65B9\u6CD5\u3092\u793A\u3059\u4F8B\u3067\u3059\uFF1A\
  \ **\u57FA\u672C\u7684\u306A\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\
  \u4F5C\u6210:**\u2026"
lastmod: '2024-03-13T22:44:42.462750-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u306F\u30D5\u30A1\u30A4\u30EB\u51E6\u7406\u306E\u305F\u3081\u306E\
  \u76F4\u611F\u7684\u306A\u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\u3092\u63D0\u4F9B\
  \u3057\u3066\u3044\u307E\u3059\u3002`Out-File` \u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\
  \u30C8\u3068\u30EA\u30C0\u30A4\u30EC\u30AF\u30B7\u30E7\u30F3\u6F14\u7B97\u5B50\u306F\
  \u3001\u3053\u306E\u76EE\u7684\u306E\u305F\u3081\u306B\u4E3B\u306B\u4F7F\u7528\u3055\
  \u308C\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u7570\u306A\u308B\u30B7\u30CA\u30EA\
  \u30AA\u3067\u30D5\u30A1\u30A4\u30EB\u306B\u30C6\u30AD\u30B9\u30C8\u3092\u66F8\u304D\
  \u8FBC\u3080\u65B9\u6CD5\u3092\u793A\u3059\u4F8B\u3067\u3059\uFF1A\n\n**\u57FA\u672C\
  \u7684\u306A\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210:**\n\
  \n\u30B7\u30F3\u30D7\u30EB\u306A\u6587\u5B57\u5217\u3092\u542B\u3080\u30C6\u30AD\
  \u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3059\u308B\u306B\u306F\u3001\
  \u4EE5\u4E0B\u3092\u4F7F\u7528\u3057\u307E\u3059\uFF1A."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 方法:
PowerShellはファイル処理のための直感的なコマンドレットを提供しています。`Out-File` コマンドレットとリダイレクション演算子は、この目的のために主に使用されます。以下は、異なるシナリオでファイルにテキストを書き込む方法を示す例です：

**基本的なテキストファイルの作成:**

シンプルな文字列を含むテキストファイルを作成するには、以下を使用します：

```powershell
"Hello, World!" | Out-File -FilePath .\example.txt
```

またはリダイレクション演算子を使用して同様に：

```powershell
"Hello, World!" > .\example.txt
```

**既存のファイルにテキストを追加する:**

既存のファイルの末尾にテキストを追加したいが、上書きしたくない場合は：

```powershell
"Another line." | Out-File -FilePath .\example.txt -Append
```

または追加するためのリダイレクション演算子を使用して：

```powershell
"Another line." >> .\example.txt
```

**複数行の書き込み:**

複数行を書き込むには、文字列の配列を使用します：

```powershell
$lines = "Line 1", "Line 2", "Line 3"
$lines | Out-File -FilePath .\multilines.txt
```

**エンコーディングを指定する:**

特定のテキストエンコーディングを指定するには、`-Encoding` パラメータを使用します：

```powershell
"Text with UTF8 Encoding" | Out-File -FilePath .\utfexample.txt -Encoding UTF8
```

**サードパーティのライブラリを使用する:**

PowerShellの組み込みコマンドレットで基本的なファイル操作には十分ですが、より複雑なタスクには`PowershellGet`やWindows用にポーティングされた`SED`や`AWK`のようなサードパーティのモジュールやツールが役立つかもしれません。しかし、単にテキストファイルを書くためだけなら、これらは通常必要なく、過剰かもしれません：

```powershell
# より複雑なシナリオが外部ライブラリの使用を正当化すると仮定する
# Install-Module -Name SomeComplexLibrary
# Import-Module -Name SomeComplexLibrary
# ここでより複雑な操作を行う
```

_注：サードパーティの依存関係を追加する複雑さがあなたのニーズに正当化されるかどうか常に考えてください。_

**サンプル出力:**

基本的なファイル作成コマンドを実行した後、`example.txt`の内容を確認すると：

```plaintext
Hello, World!
```

テキストを追加してから`example.txt`を確認すると：

```plaintext
Hello, World!
Another line.
```
