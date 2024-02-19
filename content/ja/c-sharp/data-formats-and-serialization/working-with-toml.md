---
aliases:
- /ja/c-sharp/working-with-toml/
date: 2024-01-26 04:20:32.055725-07:00
description: "TOML\u306FTom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\u305D\
  \u306E\u660E\u5FEB\u306A\u30BB\u30DE\u30F3\u30C6\u30A3\u30AF\u30B9\u306E\u304A\u304B\
  \u3052\u3067\u8AAD\u307F\u3084\u3059\u3044\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u5F62\
  \u5F0F\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u30D5\
  \u30A1\u30A4\u30EB\u306B\u4F7F\u7528\u3057\u3001\u30B7\u30B9\u30C6\u30E0\u9593\u306E\
  \u30C7\u30FC\u30BF\u4EA4\u63DB\u3092\u7C21\u7D20\u5316\u3057\u3001\u4EBA\u9593\u306E\
  \u53EF\u8AAD\u6027\u3068\u30DE\u30B7\u30F3\u306E\u89E3\u6790\u53EF\u80FD\u6027\u306E\
  \u9593\u306E\u30D0\u30E9\u30F3\u30B9\u3092\u53D6\u308B\u305F\u3081\u306B\u5229\u7528\
  \u3057\u3066\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.938953
model: gpt-4-0125-preview
summary: "TOML\u306FTom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\u305D\u306E\
  \u660E\u5FEB\u306A\u30BB\u30DE\u30F3\u30C6\u30A3\u30AF\u30B9\u306E\u304A\u304B\u3052\
  \u3067\u8AAD\u307F\u3084\u3059\u3044\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u5F62\u5F0F\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u30D5\u30A1\
  \u30A4\u30EB\u306B\u4F7F\u7528\u3057\u3001\u30B7\u30B9\u30C6\u30E0\u9593\u306E\u30C7\
  \u30FC\u30BF\u4EA4\u63DB\u3092\u7C21\u7D20\u5316\u3057\u3001\u4EBA\u9593\u306E\u53EF\
  \u8AAD\u6027\u3068\u30DE\u30B7\u30F3\u306E\u89E3\u6790\u53EF\u80FD\u6027\u306E\u9593\
  \u306E\u30D0\u30E9\u30F3\u30B9\u3092\u53D6\u308B\u305F\u3081\u306B\u5229\u7528\u3057\
  \u3066\u3044\u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
---

{{< edit_this_page >}}

## 何となぜ？
TOMLはTom's Obvious, Minimal Languageの略で、その明快なセマンティクスのおかげで読みやすい設定ファイル形式です。プログラマーは設定ファイルに使用し、システム間のデータ交換を簡素化し、人間の可読性とマシンの解析可能性の間のバランスを取るために利用しています。

## 方法：
まず、`Tomlyn`のようなTOMLパーサーをインストールします。パッケージマネージャーを使います：

```csharp
dotnet add package Tomlyn
```

次に、TOMLファイルを解析します：

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"所有者: {tomlTable["owner"]["name"]}");
// 出力：
// 所有者: Tom Preston-Werner
```

今度は、TOMLを作成し、書き込みます：

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOMLをconfig.tomlに書き込みました");
// 出力：
// TOMLをconfig.tomlに書き込みました
```

## 深掘り：
TOMLは2013年頃に、GitHubの共同設立者であるTom Preston-Wernerによって、YAMLやJSONのような既存の形式の設定における限界に対する反応として作られました。設定用に特化しており、直截明快で曖昧さのないことを重視しています。

代替設定形式にはYAML、JSON、XMLがあります。しかし、特に手作業での編集が一般的な設定ファイルにおいて、TOMLはより人に優しい形式で際立っています。JSONは普及していますが、複雑な設定では読みにくく、XMLは冗長です。YAMLは可読性においては似ていますが、空白の使用が多くなると複雑になり、特定の内容ではセキュリティリスクがあります。

実装面では、TOMLはハッシュテーブルへのクリーンなマッピングに焦点を当てており、データの抽出が予測可能です。バージョン1.0.0がリリースされると、TOMLはその仕様を固め、安定性とツールのサポートを改善しました。

## 参照：
- 公式のTOML GitHubリポジトリ＆スペック：[github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- .NETライブラリのTomlyn：[github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
