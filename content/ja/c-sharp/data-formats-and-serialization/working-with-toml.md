---
date: 2024-01-26 04:20:32.055725-07:00
description: "\u65B9\u6CD5\uFF1A \u307E\u305A\u3001`Tomlyn`\u306E\u3088\u3046\u306A\
  TOML\u30D1\u30FC\u30B5\u30FC\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\
  \u3059\u3002\u30D1\u30C3\u30B1\u30FC\u30B8\u30DE\u30CD\u30FC\u30B8\u30E3\u30FC\u3092\
  \u4F7F\u3044\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:38:41.696944-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A \u307E\u305A\u3001`Tomlyn`\u306E\u3088\u3046\u306ATOML\u30D1\
  \u30FC\u30B5\u30FC\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\u3059\u3002\
  \u30D1\u30C3\u30B1\u30FC\u30B8\u30DE\u30CD\u30FC\u30B8\u30E3\u30FC\u3092\u4F7F\u3044\
  \u307E\u3059\uFF1A."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

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
