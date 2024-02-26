---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:09.279202-07:00
description: "PowerShell\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\
  \u66F8\u304F\u3053\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u30D9\u30FC\u30B9\u306E\
  \u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3057\u64CD\u4F5C\u3059\u308B\u57FA\u672C\
  \u7684\u306A\u64CD\u4F5C\u3067\u3042\u308A\u3001\u30ED\u30B0\u8A18\u9332\u3001\u30C7\
  \u30FC\u30BF\u4FDD\u5B58\u3001\u8A2D\u5B9A\u30B9\u30AF\u30EA\u30D7\u30C8\u4F5C\u6210\
  \u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\
  \u308C\u3092\u5229\u7528\u3057\u3066\u30B7\u30B9\u30C6\u30E0\u30BF\u30B9\u30AF\u306E\
  \u81EA\u52D5\u5316\u3001\u30C7\u30FC\u30BF\u5206\u6790\u3001\u4ED6\u306E\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3084\u30B9\u30AF\u30EA\u30D7\u30C8\u3068\u306E\
  \u7D71\u5408\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.430921-07:00'
model: gpt-4-0125-preview
summary: "PowerShell\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\
  \u304F\u3053\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u30D9\u30FC\u30B9\u306E\u30D5\
  \u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3057\u64CD\u4F5C\u3059\u308B\u57FA\u672C\u7684\
  \u306A\u64CD\u4F5C\u3067\u3042\u308A\u3001\u30ED\u30B0\u8A18\u9332\u3001\u30C7\u30FC\
  \u30BF\u4FDD\u5B58\u3001\u8A2D\u5B9A\u30B9\u30AF\u30EA\u30D7\u30C8\u4F5C\u6210\u306B\
  \u4E0D\u53EF\u6B20\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\u308C\
  \u3092\u5229\u7528\u3057\u3066\u30B7\u30B9\u30C6\u30E0\u30BF\u30B9\u30AF\u306E\u81EA\
  \u52D5\u5316\u3001\u30C7\u30FC\u30BF\u5206\u6790\u3001\u4ED6\u306E\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u3084\u30B9\u30AF\u30EA\u30D7\u30C8\u3068\u306E\u7D71\
  \u5408\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？
PowerShellでテキストファイルを書くことは、テキストベースのファイルを作成し操作する基本的な操作であり、ログ記録、データ保存、設定スクリプト作成に不可欠です。プログラマはこれを利用してシステムタスクの自動化、データ分析、他のアプリケーションやスクリプトとの統合を行います。

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
